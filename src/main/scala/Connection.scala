package xyz.hyperreal.rdb

import scala.collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.lia.{FunctionMap, Math}
import xyz.hyperreal.importer.{Importer, Table, Column => ImpColumn}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]
	val variables = new HashMap[String, AnyRef]

	variables ++= Builtins.aggregateFunctions
	variables ++= Builtins.scalarFunctions
	variables ++= Builtins.constants

	def loadFromFile( file: String ): Unit = {
		val imp = new Importer

		def types( t: String ) =
			if (t == "currency")
				DecimalType
			else
				Type.names(t)

		for ((_, Table(name, header, data)) <- imp.importFromFile( file )) {
			val t = createTable( name, header map {case ImpColumn( col, typ ) => BaseRelationColumn( name, col, types(typ), None, false, false )} ) //todo: first field should be primary key, createTable should check everything, Importer should allow column info to be given in a general way like comma separated list of strings

			for (row <- data)
				t.insertRow( row )
		}
	}

	def createTable( name: String, definition: Seq[BaseRelationColumn] ) = {
		if (baseRelations contains name)
			sys.error( s"base relation '$name' already exists" )
		else if (variables contains name)
			sys.error( s"variable relation '$name' already exists" )

		val res = new BaseRelation( name, definition )

		baseRelations( name ) = res
		res
	}

	def executeRQLStatement( statement: String ): StatementResult = executeStatement( RQLParser.parseStatement(statement) )

	def executeSQLStatement( statement: String ): StatementResult = executeStatement( SQLParser.parseStatement(statement) )

	def executeStatement( ast: AST ): StatementResult =
		ast match {
			case CreateBaseRelationStatement( Ident(pos, base), columns ) =>
				if (baseRelations contains base)
					problem( pos, "a base relation by that name already exists" )
				else if (variables contains base)
					problem( pos, "a variable relation by that name already exists" )

				var hset = Set[String]()
				var pk = false

				for (ColumnDef( Ident(p, n), _, _, pkpos, _, _, _ ) <- columns)
					if (hset( n ))
						problem( p, "duplicate column" )
					else {
						hset += n

						if (pkpos != null)
							if (pk)
								problem( pkpos, "a second primary key is not allowed" )
							else
								pk = true
					}

				if (!pk)
					problem( pos, "one of the columns must be declared to be the primary key" )

				val types: Array[Type] =
					columns map {
						case ColumnDef( _, _, None, _, _, _, _ ) => null
						case ColumnDef( _, p, Some(t), _, _, _, _ ) =>
							Type.names.getOrElse( t, problem( p, s"unrecognized type name '$t'" ) )
					} toArray
				val header =
					columns zip types map {
						case (ColumnDef( _, p, _, _, _, _, _ ), null) => problem( p, "missing type specification in relation with missing values" )
						case (ColumnDef( Ident(_, n), _ , _, pkpos, _, _, u), t) => BaseRelationColumn( base, n, t, if (pkpos ne null) Some(PrimaryKey) else None, u, false )
					}

				createTable( base, header )
				CreateResult( base )
			case DropTableStatement( Ident(pos, name) ) =>
				if (baseRelations contains name)
					baseRelations remove name
				else
					problem( pos, "no base relation by that name exists" )

				DropResult( name )
			case AssignRelationStatement( Ident(pos, name), relation ) =>
				if (baseRelations contains name)
					problem( pos, "a base relation by that name already exists" )

				val rel = evalRelation( relation, Nil )
				val res = AssignResult( name, variables contains name, rel size )

				variables(name) = rel
				res
			case InsertTupleStatement( base, tuple ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "unknown base relation" )
					case Some( b ) =>
						val types = b.metadata.baseRelationHeader map (_.typ) toArray
						val t = evalTuple( types, tuple )

						(b.metadata.baseRelationHeader zip t) zip tuple.t find {case ((c, v), _) => (c.unmarkable || c.constraint.contains( PrimaryKey )) && v.isInstanceOf[Mark]} match {
							case None =>
							case Some( ((BaseRelationColumn(table, column, _, _, _, _), _), e) ) => problem( e.pos, s"column '$column' of table '$table' is unmarkable" )
						}

						b.insertRow( t ) match {
							case None => InsertResult( Nil, 0 )
							case Some( a ) => InsertResult( List(a), 1 )
						}
				}
			case InsertTupleseqStatement( base, tupleseq ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "unknown base relation" )
					case Some( b ) =>
						val types = b.metadata.header map (_.typ) toArray
						val seq = evalTupleseq( types, tupleseq, Nil )
						val (l, c) = b.insertTupleseq( seq )

						InsertResult( l, c )
				}
			case InsertRelationStatement( Ident(pos, name), relation ) =>
				val src = evalRelation( relation, Nil )

				baseRelations get name match {
					case None => problem( pos, "unknown base relation" )
					case Some( b ) =>
						if (!src.metadata.attributes.subsetOf( b.metadata.attributes ))
							problem( relation.pos, "attributes must be a subset of base" )

						val (l, c) = b.insertRelation( src )

						InsertResult( l, c )
				}
			case DeleteStatement( base, condition ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "unknown base relation" )
					case Some( b ) =>
						val afuse = AFUseOrField( NoFieldOrAFUsed )
						val cond = evalLogical( afuse, List(b.metadata), condition )

						aggregateCondition( b, cond, afuse.state )
						DeleteResult( b.delete(this, cond) )
				}
			case UpdateStatement( base, condition, updates ) =>
				baseRelations get base.name match {
					case None => problem( base.pos, "unknown base relation" )
					case Some( b ) =>
						val afuse = AFUseOrField( NoFieldOrAFUsed )
						val cond = evalLogical( afuse, List(b.metadata), condition )
						val upds =
							for ((col, expr) <- updates)
								yield {
									b.metadata.columnMap get col.name match {
										case None => problem( col.pos, "unknown column" )
										case Some( ind ) =>
											(ind, evalExpression( AFUseNotAllowed, List(b.metadata), expr ))
									}
								}

						aggregateCondition( b, cond, afuse.state )
						UpdateResult( b.update(this, cond, upds) )
				}
			case r: RelationExpression =>
				RelationResult( evalRelation(r, Nil) )
			case t: TupleseqExpression =>
				TupleseqResult( evalTupleseq(null, t, Nil) )
		}

	def evalTupleseq( types: Array[Type], tupleseq: TupleseqExpression, context: List[Metadata] ): Tupleseq = {
		tupleseq match {
			case TupleseqLit( data ) =>
				val types1 =
					if (types eq null)
						new Array[Type]( data.length )
					else
						types

				new ConcreteTupleseq( types1, evalTupleList(types1, data) )
			case SortedTupleseqExpression( relation, names, ascending ) =>
				val rel = evalRelation( relation, context )
				val fields =
					names map {
						case Ident( pos, name ) =>
							rel.metadata.columnMap get name match {
								case None => problem( pos, "unknown column" )
								case Some( ind ) => ind
							}
					}

				new SortedTupleseq( rel, fields, ascending )
		}
	}

	def evalTuple( types: Array[Type], tuple: TupleExpression ) = {
		val row = new ArrayBuffer[AnyRef]
		val r = tuple.t

		if (r.length < types.length)
			problem( tuple.pos, "not enough values")

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

		row toVector
	}

	def evalTupleList( types: Array[Type], data: List[TupleExpression] ): List[Tuple] = {
		val body = new ArrayBuffer[Tuple]

		for (t@TupleExpression( r ) <- data)
			body += evalTuple( types, t )

		body.toList
	}

	def evalRelation( ast: RelationExpression, context: List[Metadata] ): Relation = {
		ast match {
			case GroupingRelationExpression( relation, discriminator, filter, cpos, columns ) =>
				val rel = evalRelation( relation, context )//todo (nested): don't know if this is right
				val disafuse = AFUseOrField( NoFieldOrAFUsed )
				val dis = discriminator map (evalExpression(disafuse, rel.metadata :: context, _)) toVector
				val dismetadata = new Metadata( dis map (c => SimpleColumn("", c.heading, c.typ)) ) :: context
				val filtafuse = AFUseOrField( NoFieldOrAFUsed )
				val filt = filter map (evalLogical( filtafuse, dismetadata, rel.metadata, _ ))
				val colafuse = AFUseOrField( NoFieldOrAFUsed )
				val cols = columns map (evalExpression(colafuse, dismetadata, rel.metadata, _)) toVector

				if (cols isEmpty)
					problem( cpos, "at least one expression must be given for grouping" )

				new GroupingRelation( this, rel, disafuse.state, dis, filtafuse.state, filt, colafuse.state, cols )
			case ProjectionRelationExpression( relation, columns ) =>
				val rel = evalRelation( relation, context )
				val afuse = AFUseOrField( NoFieldOrAFUsed )

				new ProjectionRelation( this, rel, columns map (evalExpression(afuse, rel.metadata :: context, _)) toVector, afuse.state )
			case InnerJoinRelationExpression( left, condition, right ) =>
				val lrel = evalRelation( left, context )
				val rrel = evalRelation( right, context )
				val metadata = new Metadata( lrel.metadata.header ++ rrel.metadata.header )

				new InnerJoinRelation( this, metadata, lrel, evalLogical(AFUseNotAllowed, metadata :: context, condition), rrel )
			case SelectionRelationExpression( relation, condition ) =>
				val rel = evalRelation( relation, context )
				val afuse = AFUseOrField( NoFieldOrAFUsed )
				val cond = evalLogical( afuse, rel.metadata :: context, condition )

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
			case ListRelationExpression( columns, data ) =>
				var hset = Set[String]()

				for (ColumnSpec( Ident(p, n), _, _ ) <- columns)
					if (hset( n ))
						problem( p, "duplicate column" )
					else {
						hset += n
					}

				val types: Array[Type] =
					columns map {
						case ColumnSpec( _, _, None ) => null
						case ColumnSpec( _, p, Some(t) ) =>
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
					columns zip types map {
						case (ColumnSpec( _, p, _ ), null) => problem( p, "missing type specification in relation with missing values" )
						case (ColumnSpec( Ident(_, n), _ , _ ), t) => SimpleColumn( tab, n, t )
					}

				new ConcreteRelation( header toIndexedSeq, body )
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

		sys.error( s"not found: $p1, $c1")
	}

	def evalExpression( afuse: AggregateFunctionUse, metadata: List[Metadata], ast: ValueExpression ): ValueResult =
		evalExpression( afuse, metadata, if (metadata eq null) null else metadata.head, ast )	//todo (nested): aggregates can't see outer scopes

	def evalExpression( afuse: AggregateFunctionUse, fmetadata: List[Metadata], ametadata: Metadata, ast: ValueExpression ): ValueResult =
		ast match {
			case AliasValueExpression( expr, alias ) =>
				val v = evalExpression( afuse, fmetadata, ametadata, expr )

				AliasValue( v.pos, v.table, alias.name, v.typ, alias.pos, v )
			case FloatLit( n ) => LiteralValue( ast.pos, null, n, FloatType, java.lang.Double.valueOf(n) )
			case IntegerLit( n ) => LiteralValue( ast.pos, null, n, IntegerType, Integer.valueOf(n) )
			case StringLit( s ) => LiteralValue( ast.pos, null, '"' + s + '"', StringType, s )
			case MarkLit( m ) => MarkedValue( ast.pos, null, m.toString, null, m )
			case ValueVariableExpression( n ) =>
				search( fmetadata )( _.columnMap get n.name ) match {
					case None =>
						variables get n.name match {
							case None =>
								problem( n.pos, "no such column or variable" )
							case Some( v ) =>
								VariableValue( n.pos, null, n.name, Type.fromValue(v).orNull, v )//todo: handle function types correctly
						}
					case Some( (ind, depth) ) =>
						afuse match {
							case use@AFUseOrField( OnlyAFUsed ) => use.state = FieldAndAFUsed
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyFieldUsed
							case AFUseNotAllowed|AFUseOrField( OnlyFieldUsed|FieldAndAFUsed ) =>
						}

						FieldValue( ast.pos, fmetadata(depth).header(ind).table, n.name, fmetadata(depth).header(ind).typ, ind, depth )
				}
			case ValueColumnExpression( t, c ) =>
				if (!fmetadata.exists(_.tableSet(t.name)))
					problem( t.pos, "unknown table" )
				else
					search( fmetadata )( _.tableColumnMap get (t.name, c.name) ) match {
						case None => problem( c.pos, "no such column" )
						case Some( (ind, depth) ) =>
							afuse match {
								case use@AFUseOrField( OnlyAFUsed ) => use.state = FieldAndAFUsed
								case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyFieldUsed
								case AFUseNotAllowed|AFUseOrField( OnlyFieldUsed|FieldAndAFUsed ) =>
							}

							FieldValue( ast.pos, t.name, c.name, fmetadata(depth).header(ind).typ, ind, depth )
					}
			case e@BinaryValueExpression( left, oppos, operation, func, right ) =>
				val l = evalExpression( afuse, fmetadata, ametadata, left )
				val r = evalExpression( afuse, fmetadata, ametadata, right )

				(l, r) match {
					case (LiteralValue(p, _, _, _, x), LiteralValue(_, _, _, _, y)) =>
						val res = Math( func, x, y )

						LiteralValue( p, null, res.toString, Type.fromValue(res).get, res )
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

						BinaryValue( oppos, null, s"$lh$space$operation$space$rh", l.typ, l, operation, func, r )//todo: handle type promotion correctly
				}
			case UnaryValueExpression( oppos, operation, func, expr ) =>
				val e = evalExpression( afuse, fmetadata, ametadata, expr )

				e match {
					case LiteralValue( p, _, _, _, x ) =>
						val res = Math( func, x )

						LiteralValue( p, null, res.toString, Type.fromValue(res).get, res )
					case _ => UnaryValue( oppos, null, s"$operation${e.heading}", e.typ, e, operation, func ) //todo: handle type promotion correctly
				}
			case e@ApplicativeValueExpression( func, args ) =>
				val f = evalExpression( afuse, fmetadata, ametadata, func )

				f match {
					case VariableValue( _, _, _, _, af: AggregateFunction ) =>
						afuse match {
							case AFUseNotAllowed => problem( e.pos, "aggregate function not allowed here" )
							case use@AFUseOrField( OnlyFieldUsed ) => use.state = FieldAndAFUsed
							case use@AFUseOrField( NoFieldOrAFUsed ) => use.state = OnlyAFUsed
							case AFUseOrField( OnlyAFUsed|FieldAndAFUsed ) =>
						}

						val a = args map (evalExpression( AFUseNotAllowed, List(ametadata), null, _ ))
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}(${a map (_.heading) mkString ","})"

						AggregateFunctionValue( e.pos, null, heading, af.typ(a map (_.typ)), af, a )
					case VariableValue( _, _, _, _, sf: ScalarFunction ) =>
						val a = args map (evalExpression( afuse, fmetadata, ametadata, _ ))
						val heading =
							if (a == Nil)
								s"${f.heading}()"
							else
								s"${f.heading}(${a map (_.heading) mkString ","})"

						ScalarFunctionValue(e.pos, null, heading, sf.typ(a map (_.typ)), sf, a )
					case _ => problem( e.pos, s"'$f' is not a function" )
				}
			case e@LogicalValueExpression( logical ) =>
				val log = evalLogical( afuse, fmetadata, ametadata, logical )//todo: this might not be right if there are aggregates in a boolean expression

				LogicalValue( e.pos, null, log.heading, LogicalType, log )
		}

	def aggregateCondition( tuples: Iterable[Tuple], condition: LogicalResult, afuse: AggregateFunctionUseState ) =
		if (afuse == OnlyAFUsed || afuse == FieldAndAFUsed) {
			initAggregation( condition )

			for (t <- tuples.iterator)
				aggregate( t, condition )
		}

	def aggregateColumns( tuples: Iterable[Tuple], columns: Vector[ValueResult], afuse: AggregateFunctionUseState ) =
		if (afuse == OnlyAFUsed || afuse == FieldAndAFUsed) {
			for (c <- columns)
				initAggregation( c )

			for (t <- tuples.iterator; c <- columns)
				aggregate( t, c )
		}

	def aggregate( row: Tuple, result: ValueResult ): Unit =
		result match {
			case BinaryValue( _, _, _, _, l, _, _, r ) =>
				aggregate( row, l )
				aggregate( row, r )
			case UnaryValue( _, _, _, _, v, _, _ ) =>
				aggregate( row, v )
			case a@AggregateFunctionValue( _, _, _, _, _, args ) =>
				a.func.next( args map (evalValue( List(row)/*todo (nested): this may not be right*/, _ )) )
			case ScalarFunctionValue( _, _, _, _, _, args ) =>
				for (a <- args)
					aggregate( row, a )
			case LogicalValue( _, _, _, _, l ) =>
				aggregate( row, l )
			case _ =>
		}

	def aggregate( row: Tuple, result: LogicalResult ): Unit =
		result match {
			case _: LiteralLogical =>
			case ComparisonLogical( _, left, _, _, right ) =>
				aggregate( row, left )
				aggregate( row, right )
		}

	def initAggregation( result: ValueResult ): Unit =
		result match {
			case BinaryValue( _, _, _, _, l, _, _, r ) =>
				initAggregation( l )
				initAggregation( r )
			case UnaryValue( _, _, _, _, v, _, _ ) =>
				initAggregation( v )
			case a@AggregateFunctionValue( _, _, _, _, af, _ ) =>
				a.func = af.instance
			case ScalarFunctionValue( _, _, _, _, _, args ) =>
				for (a <- args)
					initAggregation( a )
			case LogicalValue( _, _, _, _, l ) =>
				initAggregation( l )
			case _ =>
		}

	def initAggregation( result: LogicalResult ): Unit =
		result match {
			case _: LiteralLogical =>
			case ComparisonLogical( _, left, _, _, right ) =>
				initAggregation( left )
				initAggregation( right )
		}

	def evalVector( row: List[Tuple], vector: Vector[ValueResult]) = vector map (evalValue(row, _))

	def evalValue( row: List[Tuple], result: ValueResult ): AnyRef =
		result match {
			case AliasValue( _, _, _, _, _, v ) => evalValue( row, v )
			case LiteralValue( _, _, _, _, v ) => v
			case VariableValue( _, _, _, _, v ) => v
			case FieldValue( _, _, _, _, index, depth ) => row(depth)(index)
			case MarkedValue( _, _, _, _, m ) => m
			case BinaryValue( p, _, _, _, l, _, f, r ) =>
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
			case UnaryValue( p, _, _, _, v, _, f ) =>
				try {
					Math( f, evalValue(row, v) )
				} catch {
					case _: Exception => problem( p, "error performing unary operation" )
				}
			case ScalarFunctionValue( _, _, _, _, func, args ) => func( args map (evalValue( row, _ )) ).asInstanceOf[AnyRef]
			case a: AggregateFunctionValue => a.func.result.asInstanceOf[AnyRef]
			case LogicalValue( _, _, _, _, l ) => evalCondition( row, l )
		}

	def evalCondition( context: List[Tuple], cond: LogicalResult ): Logical =
		cond match {
			case ExistsLogical( _, relation ) =>
				relation match {
					case r: Relation =>
						Logical.fromBoolean( r.iterator(context).nonEmpty )
					case _ => sys.error( "fix this" )
				}

			case LiteralLogical( _, lit ) => lit
			case ComparisonLogical( _, left, _, pred, right ) =>
				val lv = evalValue( context, left )
				val rv = evalValue( context, right )

				(lv, rv) match {
					case (`I`, _)|(_, `I`) => MAYBE_I
					case (`A`, _)|(_, `A`) => MAYBE_A
					case _ =>
						if (lv.isInstanceOf[String] || rv.isInstanceOf[String]) {
							Logical.fromBoolean( Math.predicate(pred, lv.toString compareTo rv.toString, 0) )
						} else
							Logical.fromBoolean( Math.predicate(pred, lv, rv) )
				}
			case AndLogical( _, l, r ) =>
				evalCondition( context, l ) and evalCondition( context, r )
			case OrLogical( _, l, r ) =>
				evalCondition( context, l ) or evalCondition( context, r )
		}

	def evalLogical( afuse: AggregateFunctionUse, metadata: List[Metadata], ast: LogicalExpression ): LogicalResult =
		evalLogical( afuse, metadata, metadata.head, ast )//todo (nested): aggregates can't see outer scopers

	def evalLogical( afuse: AggregateFunctionUse, fmetadata: List[Metadata], ametadata: Metadata, ast: LogicalExpression ): LogicalResult = {
		ast match {
			case ExistsLogicalExpression( tuples ) =>
				val rel =
					tuples match {
						case r: RelationExpression => evalRelation( r, fmetadata )
						case s: TupleseqExpression => evalTupleseq( null, s, fmetadata )
					}

				ExistsLogical( s"exists ($rel)", rel )
			case LiteralLogicalExpression( lit ) => LiteralLogical( lit.toString, lit )
			case ComparisonLogicalExpression( left, List((comp, pred, right)) ) =>
				val l = evalExpression( afuse, fmetadata, ametadata, left )
				val r = evalExpression( afuse, fmetadata, ametadata, right )

				ComparisonLogical( s"${l.heading} $comp ${r.heading}", l, comp, pred, r )
			case ComparisonLogicalExpression( left, List((compm, predm, middle), (compr, predr, right)) ) =>
				val l = evalExpression( afuse, fmetadata, ametadata, left )
				val m = evalExpression( afuse, fmetadata, ametadata, middle )
				val r = evalExpression( afuse, fmetadata, ametadata, right )
				val lc = ComparisonLogical( s"${l.heading} $compm ${m.heading}", l, compm, predm, m )
				val rc = ComparisonLogical( s"${m.heading} $compr ${r.heading}", m, compr, predr, r )

				AndLogical( s"${lc.heading} and ${rc.heading}", lc, rc )
			case AndLogicalExpression( left, right ) =>
				val l = evalLogical( afuse, fmetadata, ametadata, left )
				val r = evalLogical( afuse, fmetadata, ametadata, right )

				AndLogical( s"${l.heading} and ${r.heading}", l, r )
			case OrLogicalExpression( left, right ) =>
				val l = evalLogical( afuse, fmetadata, ametadata, left )
				val r = evalLogical( afuse, fmetadata, ametadata, right )

				OrLogical( s"${l.heading} Or ${r.heading}", l, r )
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

trait LogicalResult {
	val heading: String
}

case class LiteralLogical( heading: String, value: Logical ) extends LogicalResult
case class AndLogical( heading: String, left: LogicalResult, right: LogicalResult ) extends LogicalResult
case class OrLogical( heading: String, left: LogicalResult, right: LogicalResult ) extends LogicalResult
case class ComparisonLogical( heading: String, left: ValueResult, comp: String, pred: FunctionMap, right: ValueResult ) extends LogicalResult
case class ExistsLogical( heading: String, tuples: Iterable[Tuple] ) extends LogicalResult

trait StatementResult
case class CreateResult( name: String ) extends StatementResult
case class DropResult( name: String ) extends StatementResult
case class AssignResult( name: String, update: Boolean, count: Int ) extends StatementResult
case class InsertResult( auto: List[Map[String, AnyRef]], count: Int ) extends StatementResult
case class DeleteResult( count: Int ) extends StatementResult
case class UpdateResult( count: Int ) extends StatementResult
case class RelationResult( relation: Relation ) extends StatementResult
case class TupleseqResult( tupleseq: Tupleseq ) extends StatementResult