package xyz.hyperreal.rdb

import scala.util.parsing.input.Position
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}

import xyz.hyperreal.lia.{FunctionMap, Math}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]
	val variables = new HashMap[String, AnyRef]

	variables ++= Builtins.aggregateFunctions
	variables ++= Builtins.scalarFunctions

	def executeStatement( statement: String ): StatementResult = {
		val p = new RQLParser
		val ast = p.parseFromString( statement, p.statement )

		ast match {
			case AssignRelationStatement( Ident(pos, name), relation ) =>
				if (baseRelations contains name)
					problem( pos, "a base relation by that name already exists" )

				val rel = evalRelation( relation )
				val res = AssignResult( name, variables contains name, rel size )

				variables(name) = rel
				res
			case InsertTupleseqStatement( base, tupleseq ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "base relation cannot be created from tuple set" )
					case Some( b ) =>
						val types = b.metadata.header map (_.typ) toArray
						val seq = evalTupleseq( types, tupleseq )
						val (l, c) = b.insertTupleseq( seq )

						InsertResult( l, c, None )
				}
			case InsertRelationStatement( Ident(pos, name), relation ) =>
				val src = evalRelation( relation )
				val (dst, created) =
					baseRelations get name match {
						case None =>
							if (variables contains name)
								problem( pos, "a variable relation by that name already exists" )

							val baserel = new BaseRelation( name, src.metadata.header )

							baseRelations(name) = baserel
							(baserel, Some( name ))
						case Some( b ) =>
							if (!src.metadata.attributes.subsetOf( b.metadata.attributes ))
								problem( relation.pos, "attributes must be a subset of base" )

							(b, None)
					}
				val (l, c) = dst.insertRelation( src )

				InsertResult( l, c, created )
			case DeleteStatement( base, condition ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "unknown base relation" )
					case Some( b ) =>
						val afuse = AFUseOrField( NoFieldOrAFUsed )
						val cond = evalLogical( afuse, b.metadata, condition )

						aggregateCondition( b, cond, afuse.state )
						DeleteResult( b.delete(this, cond) )
				}
			case r: RelationExpression =>
				RelationResult( evalRelation(r) )
			case t: TupleseqExpression =>
				TupleseqResult( evalTupleseq(null, t) )
		}
	}

	def evalTupleseq( types: Array[Type], tupleseq: TupleseqExpression ) = {
		tupleseq match {
			case TupleseqLit( data ) =>
				val types1 =
					if (types eq null)
						new Array[Type]( data.length )
					else
						types

				new ListTupleseq( types1, evalTupleList(types1, data) )
			case ProjectionTupleseqExpression( relation: RelationExpression, columns ) =>
				val rel = evalRelation( relation )
				val afuse = AFUseOrField( NoFieldOrAFUsed )
				val cols = columns map (evalExpression(afuse, rel.metadata, _))

				new ProjectionTupleseq( this, rel, cols toVector, afuse.state )
//			case AggregationTupleseqExpression( relation, discriminator, columns ) =>
//				val rel = evalRelation( relation )
//				val dis = discriminator map (evalExpression(AFUseNotAllowed, rel.metadata, _))
//
//				AggregationTupleseq
		}
	}

	def evalTupleList( types: Array[Type], data: List[TupleExpression] ): List[Tuple] = {
		val body = new ArrayBuffer[Tuple]

		for (t@TupleExpression( r ) <- data) {
			val row = new ArrayBuffer[AnyRef]

			if (r.length < types.length)
				problem( t.pos, "not enough values")

			for ((v, i) <- r zipWithIndex) {
				if (i == types.length)
					problem( v.pos, "too many values")

				var x = evalValue( null, evalExpression(AFUseNotAllowed, null, v) )

				x match {
					case _: Mark =>
					case a: java.lang.Integer =>
						types(i) match {
							case null => types(i) = IntegerType
							case IntegerType =>
							case FloatType =>
								x = a.toDouble.asInstanceOf[Number]
							case typ => problem( v.pos, s"expected $typ, not integer" )
						}
					case _: String =>
						types(i) match {
							case null => types( i ) = StringType
							case StringType =>
							case typ => problem( v.pos, s"expected $typ, not string" )
						}
				}

				row += x
			}

			body += row.toVector
		}

		body.toList
	}

	def evalRelation( ast: RelationExpression ): Relation = {
		ast match {
			case InnerJoinRelationExpression( left, condition, right ) =>
				val lrel = evalRelation( left )
				val rrel = evalRelation( right )
				val metadata = new Metadata( lrel.metadata.header ++ rrel.metadata.header )

				new InnerJoinRelation( this, metadata, lrel, evalLogical(AFUseNotAllowed, metadata, condition), rrel )
			case SelectionRelationExpression( relation, condition ) =>
				val rel = evalRelation( relation )
				val afuse = AFUseOrField( NoFieldOrAFUsed )
				val cond = evalLogical( afuse, rel.metadata, condition )

				new SelectionRelation( this, rel, cond, afuse.state )
			case RelationVariableExpression( Ident(p, n) ) =>
				baseRelations get n match {
					case None =>
						variables get n match {
							case Some( r: Relation ) => r
							case _ => problem( p, "unknown base or variable relation" )
						}

					case Some( r ) => r
				}
			case ProjectionRelationExpression( relation, columns ) =>
				val rel = evalRelation( relation )
				val s = new HashSet[String]
				val cs = new ListBuffer[String]

				for (Ident( p, n ) <- columns)
					if (!rel.metadata.columnMap.contains( n ))
						problem( p, "unknown column name" )
					else if (s(n))
						problem( p, "duplicate column name" )
					else {
						s += n
						cs += n
					}

				new ProjectionRelation( rel, cs toList )
			case ListRelationExpression( columns, data ) =>
				var hset = Set[String]()
				var pk = false

				for (ColumnSpec( Ident(p, n), _, _, pkpos, _, _ ) <- columns)
					if (hset( n ))
						problem( p, s"duplicate $n" )
					else {
						hset += n

						if (pkpos != null)
							if (pk)
								problem( pkpos, "a relation must have exactly on primary key (rs-8)" )
							else
								pk = true
					}

				val types: Array[Type] =
					columns map {
						case ColumnSpec( _, _, None, _, _, _ ) => null
						case ColumnSpec( _, p, Some(t), _, _, _ ) =>
							Type.names.getOrElse( t, problem( p, s"unrecognized type name '$t'" ) )
					} toArray
				val body =
					if (data isEmpty)
						types indexOf null match {
							case -1 => Nil
							case ind => problem( columns(ind).typepos, "missing type specification" )
						}
					else
						evalTupleList( types, data )

				val tab = anonymous
				val header =
					(columns zip types).zipWithIndex map {
						case ((ColumnSpec( _, p, _, _, _, _ ), null), _) => problem( p, "missing type specification in relation with missing values" )
						case ((ColumnSpec( Ident(_, n), _ , _, _, _, _), t), 0) if !pk => Column( tab, n, t, Some(PrimaryKey) )
						case ((ColumnSpec( Ident(_, n), _ , _, pkpos, _, _), t), _) => Column( tab, n, t, if (pkpos ne null) Some(PrimaryKey) else None )
					}

				new ListRelation( header toIndexedSeq, body )
		}
	}

	private val oporder = List( List("b^"), List("u-"), List("b*", "b/"), List("b+", "b-") )

	private def brackets( p: ValueExpression, c: ValueExpression, right: Boolean ): Boolean = {
		def s( e: ValueExpression ) =
			e match {
				case BinaryValueExpression( _, _, operation, _, _ ) => Some( 'b' + operation )
				case UnaryValueExpression( _, operation, _, _ ) => Some( 'u' + operation )
				case _ => None
			}

		val p1 = s( p )
		val c1 = s( c )

		if (c1 isEmpty)
			return false
		else
			for (l <- oporder)
				if (l contains p1.get)
					if (l contains c1.get)
						if (right)
							return true
						else
							return false
					else
						return true
				else if (l contains c1.get)
					return false

		sys.error( s"no found: $p1, $c1")
	}

	def evalExpression( afuse: AggregateFunctionUse, metadata: Metadata, ast: ValueExpression ): ValueResult =
		ast match {
			case FloatLit( n ) => LiteralValue( ast.pos, n, FloatType, java.lang.Double.valueOf(n) )
			case IntegerLit( n ) => LiteralValue( ast.pos, n, IntegerType, Integer.valueOf(n) )
			case StringLit( s ) => LiteralValue( ast.pos, '"' + s + '"', StringType, s )
			case MarkLit( m ) => MarkedValue( ast.pos, m.toString, null, m )
			case ValueVariableExpression( n ) =>
				metadata.columnMap get n.name match {
					case None =>
						variables get n.name match {
							case None => problem( n.pos, "no such column or variable" )
							case Some( v ) => VariableValue( n.pos, n.name, null, v )//todo: set type properly
//						case Some( s: ScalarFunction ) => VariableValue( n.pos, n.name, null, s )
//						case Some( a: Class[_] ) if classOf[AggregateFunction] isAssignableFrom a => VariableValue( n.pos, n.name, null, a )//todo: set type properly
						}
					case Some( ind ) =>
						afuse match {
							case use@AFUseOrField( OnlyAFUsed ) => use.state = FieldAndAFUsed
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyFieldUsed
							case AFUseNotAllowed|AFUseOrField( OnlyFieldUsed|FieldAndAFUsed ) =>
						}

						FieldValue( ast.pos, n.name, metadata.header(ind).typ, ind )
				}
			case ValueColumnExpression( t, c ) =>
				if (!metadata.tableSet(t.name))
					problem( t.pos, "unknown table" )
				else
					metadata.tableColumnMap get (t.name, c.name) match {
						case None => problem( c.pos, "no such column" )
						case Some( ind ) =>
							afuse match {
								case use@AFUseOrField( OnlyAFUsed ) => use.state = FieldAndAFUsed
								case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyFieldUsed
								case AFUseNotAllowed|AFUseOrField( OnlyFieldUsed|FieldAndAFUsed ) =>
							}

							FieldValue( ast.pos, t.name + '.' + c.name, metadata.header(ind).typ, ind )
					}
			case e@BinaryValueExpression( left, oppos, operation, func, right ) =>
				val l = evalExpression( afuse, metadata, left )
				val r = evalExpression( afuse, metadata, right )

				(l, r) match {
					case (LiteralValue(p, _, _, x), LiteralValue(_, _, _, y)) =>
						val res = Math( func, x, y )

						LiteralValue( p, res.toString, Type.fromValue(res).get, res )
					case _ =>
						val space = if (Set("+", "-")( operation )) " " else ""
						val lh =
							if (brackets(e, left, false))
								s"(${l.heading})"
							else
								l.heading
						val rh =
							if (brackets(e, right, true))
								s"(${r.heading})"
							else
								r.heading

						BinaryValue( oppos, s"$lh$space$operation$space$rh", l.typ, l, operation, func, r )//todo: handle type promotion correctly
				}
			case UnaryValueExpression( oppos, operation, func, expr ) =>
				val e = evalExpression( afuse, metadata, expr )

				e match {
					case LiteralValue( p, _, _, x ) =>
						val res = Math( func, x )

						LiteralValue( p, res.toString, Type.fromValue(res).get, res )
					case _ => UnaryValue( oppos, s"$operation${e.heading}", e.typ, e, operation, func ) //todo: handle type promotion correctly
				}
			case e@ApplicativeValueExpression( func, args ) =>
				val f = evalExpression( afuse, metadata, func )

				f match {
					case VariableValue( _, _, _, af: AggregateFunction ) =>
						afuse match {
							case AFUseNotAllowed => problem( e.pos, "aggregate function not allowed here" )
							case use@AFUseOrField( OnlyFieldUsed ) => use.state = FieldAndAFUsed
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyAFUsed
							case AFUseOrField( OnlyAFUsed|FieldAndAFUsed ) =>
						}

						val a = args map (evalExpression( AFUseNotAllowed, metadata, _ ))
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}(${a map (_.heading) mkString ","})"

						AggregateFunctionValue( e.pos, heading, af.typ(a map (_.typ)), af, a )
					case VariableValue( _, _, _, sf: ScalarFunction ) =>
						val a = args map (evalExpression( afuse, metadata, _ ))
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}(${a map (_.heading) mkString ","})"

						ScalarFunctionValue(e.pos, heading, sf.typ(a map (_.typ)), sf, a )
					case _ => problem( e.pos, s"'$f' is not a function" )
				}
			case e@LogicalValueExpression( logical ) =>
				val log = evalLogical( afuse, metadata, logical )

				LogicalValue( e.pos, log.heading, LogicalType, log )
		}

	def aggregateCondition( relation: Relation, condition: LogicalResult, afuse: AggregateFunctionUseState ) =
		if (afuse == OnlyAFUsed || afuse == FieldAndAFUsed) {
			initAggregation( condition )

			for (t <- relation.iterator)
				aggregate( t, condition )
		}

	def aggregate( row: Tuple, result: ValueResult ): Unit =
		result match {
			case BinaryValue( _, _, _, l, _, _, r ) =>
				aggregate( row, l )
				aggregate( row, r )
			case UnaryValue( _, _, _, v, _, _ ) =>
				aggregate( row, v )
			case a@AggregateFunctionValue( _, _, _, _, args ) =>
				a.func.next( args map (evalValue( row, _ )) )
			case ScalarFunctionValue( _, _, _, _, args ) =>
				for (a <- args)
					aggregate( row, a )
			case LogicalValue( _, _, _, l ) =>
				aggregate( row, l )
			case _ =>
		}

	def aggregate( row: Tuple, result: LogicalResult ): Unit =
		result match {
			case _: LiteralLogical =>
			case ComparisonLogical( _, left, _, right ) =>
				aggregate( row, left )
				aggregate( row, right )
		}

	def initAggregation( result: ValueResult ): Unit =
		result match {
			case BinaryValue( _, _, _, l, _, _, r ) =>
				initAggregation( l )
				initAggregation( r )
			case UnaryValue( _, _, _, v, _, _ ) =>
				initAggregation( v )
			case a@AggregateFunctionValue( _, _, _, af, _ ) =>
				a.func = af.instance
			case ScalarFunctionValue( _, _, _, _, args ) =>
				for (a <- args)
					initAggregation( a )
			case LogicalValue( _, _, _, l ) =>
				initAggregation( l )
			case _ =>
		}

	def initAggregation( result: LogicalResult ): Unit =
		result match {
			case _: LiteralLogical =>
			case ComparisonLogical( _, left, _, right ) =>
				initAggregation( left )
				initAggregation( right )
		}

	def evalValue( row: Tuple, result: ValueResult ): AnyRef =
		result match {
			case LiteralValue( _, _, _, v ) => v
			case FieldValue( _, _, _, index: Int ) => row(index)
			case MarkedValue( _, _, _, m ) => m
			case BinaryValue( p, _, _, l, _, f, r ) =>
				val lv = evalValue( row, l )
				val rv = evalValue( row, r )

				(lv, rv) match {
					case (I, _)|(_, I) => I
					case (A, _)|(_, A) => A
					case _ =>
						try {
							Math( f, lv, rv )
						} catch {
							case _: Exception => problem( p, "error performing binary operation" )
						}
				}
			case UnaryValue( p, _, _, v, _, f ) =>
				try {
					Math( f, evalValue(row, v) )
				} catch {
					case _: Exception => problem( p, "error performing unary operation" )
				}
			case ScalarFunctionValue( _, _, _, func, args ) => func( args map (evalValue( row, _ )) ).asInstanceOf[AnyRef]
			case a: AggregateFunctionValue => a.func.result.asInstanceOf[AnyRef]
			case LogicalValue( _, _, _, l ) => evalCondition( row, l )
		}

	def evalCondition( row: Tuple, cond: LogicalResult ): Logical =
		cond match {
			case LiteralLogical( _, lit ) => lit
			case ComparisonLogical( _, left, pred, right ) =>
				Logical.fromBoolean( Math.predicate(pred, evalValue(row, left), evalValue(row, right)) )
		}

	def evalLogical( afuse: AggregateFunctionUse, metadata: Metadata, ast: LogicalExpression ): LogicalResult = {
		ast match {
			case LogicalLit( lit ) => LiteralLogical( lit.toString, lit )
			case ComparisonExpression( left, List((comp, pred, right)) ) =>
				val l = evalExpression( afuse, metadata, left )
				val r = evalExpression( afuse, metadata, right )

				ComparisonLogical( s"${l.heading} $comp ${r.heading}", l, pred, r )
		}
	}
}

trait AggregateFunctionUseState
case object NoFieldOrAFUsed extends AggregateFunctionUseState
case object OnlyFieldUsed extends AggregateFunctionUseState
case object OnlyAFUsed extends AggregateFunctionUseState
case object FieldAndAFUsed extends AggregateFunctionUseState

trait AggregateFunctionUse
case object AFUseNotAllowed extends AggregateFunctionUse
case class AFUseOrField( var state: AggregateFunctionUseState ) extends AggregateFunctionUse

trait ValueResult {
	val pos: Position
	val heading: String
	val typ: Type
}

case class LiteralValue( pos: Position, heading: String, typ: Type, value: AnyRef ) extends ValueResult
case class VariableValue( pos: Position, heading: String, typ: Type, value: AnyRef ) extends ValueResult
case class FieldValue( pos: Position, heading: String, typ: Type, index: Int ) extends ValueResult
case class MarkedValue( pos: Position, heading: String, typ: Type, m: Mark ) extends ValueResult
case class BinaryValue( pos: Position, heading: String, typ: Type, left: ValueResult, operation: String, func: FunctionMap, right: ValueResult ) extends ValueResult
case class AggregateFunctionValue( pos: Position, heading: String, typ: Type, af: AggregateFunction, args: List[ValueResult] ) extends ValueResult {
	var func: AggregateFunctionInstance = _
	}
case class ScalarFunctionValue( pos: Position, heading: String, typ: Type, func: ScalarFunction, args: List[ValueResult] ) extends ValueResult
case class UnaryValue( pos: Position, heading: String, typ: Type, v: ValueResult, operation: String, func: FunctionMap ) extends ValueResult
case class LogicalValue( pos: Position, heading: String, typ: Type, logical: LogicalResult ) extends ValueResult

trait LogicalResult {
	val heading: String
}

case class LiteralLogical( heading: String, value: Logical ) extends LogicalResult
case class ComparisonLogical( heading: String, left: ValueResult, pred: FunctionMap, right: ValueResult ) extends LogicalResult

trait StatementResult
case class AssignResult( name: String, update: Boolean, count: Int ) extends StatementResult
case class InsertResult( auto: List[Map[String, AnyRef]], count: Int, created: Option[String] ) extends StatementResult
case class DeleteResult( count: Int ) extends StatementResult
case class RelationResult( relation: Relation ) extends StatementResult
case class TupleseqResult( tupleseq: Tupleseq ) extends StatementResult