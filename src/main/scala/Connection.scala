package xyz.hyperreal.rdb

import scala.util.parsing.input.Position

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}

import xyz.hyperreal.lia.{FunctionMap, Math}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]
	val variables = new HashMap[String, AnyRef]

	variables("count") = classOf[CountAggregateFunction]
	variables("sum") = classOf[SumAggregateFunction]
	variables("avg") = classOf[AvgAggregateFunction]
	variables("min") = classOf[MinAggregateFunction]
	variables("max") = classOf[MaxAggregateFunction]
	variables("list") = classOf[ListAggregateFunction]

	variables("pi") = BuiltinScalarFunctions.piFunction
	variables("abs") = BuiltinScalarFunctions.absFunction
	variables("sqrt") = BuiltinScalarFunctions.sqrtFunction

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
						val cond = evalLogical( AFUseNotAllowed, b.metadata, condition )

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
				val cond = evalLogical( AFUseNotAllowed, rel.metadata, condition )

				new SelectionRelation( this, rel, cond )
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
							case AFUseOrField( AFUsed ) => problem( n.pos, "column not allowed here (since aggregate function already referred to)" )
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = FieldUsed
							case AFUseNotAllowed =>
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
								case AFUseOrField( AFUsed ) => problem( t.pos, "column not allowed here (since aggregate function already referred to)" )
								case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = FieldUsed
							}

							FieldValue( ast.pos, t.name + '.' + c.name, metadata.header(ind).typ, ind )
					}
			case BinaryValueExpression( left, oppos, operation, func, right ) =>
				val l = evalExpression( afuse, metadata, left )
				val r = evalExpression( afuse, metadata, right )

				(l, r) match {
					case (LiteralValue(p, _, _, x), LiteralValue(_, _, _, y)) =>
						val res = Math( func, x, y )

						LiteralValue( p, res.toString, Type.fromValue( res ).get, res )
					case _ => BinaryValue( oppos, s"${l.heading} $operation ${r.heading}", l.typ, l, operation, func, r )//todo: handle type promotion correctly
				}
			case NegativeValueExpression( expr, func ) =>
				val ex = evalExpression( afuse, metadata, expr )

				ex match {
					case LiteralValue( p, _, _, x ) =>
						val res = Math( func, x )

						LiteralValue( p, res.toString, Type.fromValue( res ).get, res )
					case _ => BinaryValue( oppos, s"${l.heading} $operation ${r.heading}", l.typ, l, operation, func, r ) //todo: handle type promotion correctly
				}
			case e@ApplicativeValueExpression( func, args ) =>
				val f = evalExpression( afuse, metadata, func )
				val a = args map (evalExpression( AFUseNotAllowed, metadata, _ ))

				f match {
					case VariableValue( _, _, _, afc: Class[_] ) if classOf[AggregateFunction] isAssignableFrom afc =>
						afuse match {
							case AFUseNotAllowed => problem( e.pos, "aggregate function not allowed here" )
							case AFUseOrField( FieldUsed ) => problem( e.pos, "aggregate function not allowed here (since column already referred to)" )
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = AFUsed
						}

						val af = afc.newInstance.asInstanceOf[AggregateFunction]
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}( ${a map (_.heading) mkString ","} )"

						AggregateFunctionValue( e.pos, heading, af.typ(a map (_.typ)), af, a )
//					case sf: ScalarFunction =>
					case VariableValue( _, _, _, sf: ScalarFunction ) =>
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}( ${a map (_.heading) mkString ","} )"

						ScalarFunctionValue(e.pos, heading, sf.typ(a map (_.typ)), sf, a )
					case _ => problem( e.pos, s"'$f' is not a function" )
				}
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
							case _: Exception => problem( p, "error performing operation" )
						}
				}
			case ScalarFunctionValue( _, _, _, func, args ) => func( args map (evalValue( row, _ )) )
			case AggregateFunctionValue( _, _, _, func, args ) if row eq null => func.result
			case AggregateFunctionValue( _, _, _, func, args ) =>
				func.next( args map (evalValue( row, _ )) )
				A
		}

	def evalCondition( row: Tuple, cond: ConditionResult ): Boolean =
		cond match {
			case ComparisonLogical( left, pred, right ) =>
				Math.predicate( pred, evalValue(row, left), evalValue(row, right) )
		}

	def evalLogical( afuse: AggregateFunctionUse, metadata: Metadata, ast: LogicalExpression ): ConditionResult = {
		ast match {
			case ComparisonExpression( left, List((_, pred, right)) ) =>
				val l = evalExpression( afuse, metadata, left )
				val r = evalExpression( afuse, metadata, right )

				ComparisonLogical( l, pred, r )
		}
	}
}

trait AggregateFunctionUseState
case object NoFieldOrAFUsed extends AggregateFunctionUseState
case object FieldUsed extends AggregateFunctionUseState
case object AFUsed extends AggregateFunctionUseState

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
case class AggregateFunctionValue( pos: Position, heading: String, typ: Type, func: AggregateFunction, args: List[ValueResult] ) extends ValueResult
case class ScalarFunctionValue( pos: Position, heading: String, typ: Type, func: ScalarFunction, args: List[ValueResult] ) extends ValueResult

trait ConditionResult
case class ComparisonLogical( left: ValueResult, pred: FunctionMap, right: ValueResult ) extends ConditionResult

trait StatementResult
case class AssignResult( name: String, update: Boolean, count: Int ) extends StatementResult
case class InsertResult( auto: List[Map[String, AnyRef]], count: Int, created: Option[String] ) extends StatementResult
case class DeleteResult( count: Int ) extends StatementResult
case class RelationResult( relation: Relation ) extends StatementResult
case class TupleseqResult( tupleseq: Tupleseq ) extends StatementResult