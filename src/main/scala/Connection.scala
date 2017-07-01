package xyz.hyperreal.rdb

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import xyz.hyperreal.lia.{FunctionMap, Math}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]

	def executeStatement( statement: String ): StatementResult = {
		val p = new RQLParser
		val ast = p.parseFromString( statement, p.statement )

		ast match {
			case InsertTuplesetStatement( target, tupleset ) =>
				baseRelations get target.name match {
					case None => problem( target.pos, "base relation cannot be created from tuple set" )
					case Some( base ) =>
						val types = base.header map (_.typ) toArray
						val body = evalTupleset( types, tupleset )
						val (l, c) = base.insertTupleset( body )

						InsertResult( l, c, None )
				}
			case InsertRelationStatement( target, relation ) =>
				val src = evalRelation( relation )
				val (dst, created) =
					baseRelations get target.name match {
						case None =>
							val base = new BaseRelation( target.name, src.header )

							baseRelations(target.name) = base
							(base, Some( target.name ))
						case Some( base ) =>
							if (!src.headerSet.subsetOf( base.headerSet ))
								problem( relation.pos, "attributes must be a subset of target" )

							(base, None)
					}
				val (l, c) = dst.insertRelation( src )

				InsertResult( l, c, created )
			case r: RelationExpression =>
				RelationResult( evalRelation(r) )
		}
	}

	def evalTupleset( types: Array[Type], data: List[List[ValueExpression]] ): List[Vector[AnyRef]] = {
		val body = new ArrayBuffer[Vector[AnyRef]]

		for (r <- data) {
			val row = new ArrayBuffer[AnyRef]

			if (r.length < types.length)
				problem( r.last.pos, "unexpected last value (row too short)")

			for ((v, i) <- r zipWithIndex) {
				if (i == types.length)
					problem( v.pos, "too many values")

				var x = evalValue( null, evalExpression(null, v) )

				x match {
					case _: Mark =>
					case a: java.lang.Integer =>
						types(i) match {
							case null => types(i) = IntegerType
							case IntegerType =>
							case FloatType =>
								x = a.toDouble.asInstanceOf[Number]
							case t => problem( v.pos, s"expected $t, not integer" )
						}
					case _: String =>
						types(i) match {
							case null => types( i ) = StringType
							case StringType =>
							case t => problem( v.pos, s"expected $t, not string" )
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
				val lmap = lrel.columnNameMap
				val lmapsize = lmap.size
				val rrel = evalRelation( right )
				val shiftedrmap = rrel.columnNameMap map {case (k, v) => (k, v + lmapsize)}

				new InnerJoinRelation( this, lrel, evalLogical(lmap ++ shiftedrmap, condition), rrel )
			case SelectionRelationExpression( relation, condition ) =>
				val rel = evalRelation( relation )
				val cond = evalLogical( rel.columnNameMap, condition )

				new SelectionRelation( this, rel, cond )
			case RelationVariableExpression( Ident(p, n) ) =>
				baseRelations get n match {
					case None => problem( p, "unknown base relation" )
					case Some( r ) => r
				}
			case ProjectionRelationExpression( relation, columns ) =>
				val rel = evalRelation( relation )
				val s = new HashSet[String]
				val cs = new ListBuffer[String]

				for (Ident( p, n ) <- columns)
					if (!rel.columnNameMap.contains( n ))
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

				for (ColumnSpec( Ident(p, n), _, _, pk ) <- columns)
					if (hset(n))
						problem( p, s"duplicate $n" )
					else
						hset += n

				val types: Array[Type] =
					columns map {
						case ColumnSpec( _, _, None, pk ) => null
						case ColumnSpec( _, p, Some(t), pk ) => Type.fromSpec( p, t )
					} toArray
				val body =
					if (data isEmpty)
						types indexOf null match {
							case -1 => Nil
							case ind => problem( columns(ind).typepos, "missing type specification in empty relation" )
						}
					else
						evalTupleset( types, data )

				val tab = "_" + anonymous
				val header =
					columns zip types map {
						case (ColumnSpec( _, p, _, pk ), null) => problem( p, "missing type specification in relation with missing values" )
						case (ColumnSpec( Ident(_, n), _ , _, pk), t) => Column( tab, n, t )
					}

				new ListRelation( header toIndexedSeq, body )
		}
	}

	def evalExpression( columnNameMap: Map[String, Int], ast: ValueExpression ): ValueResult =
		ast match {
			case FloatLit( n ) => NumberValue( java.lang.Double.parseDouble(n) )
			case IntegerLit( n ) => NumberValue( Integer.parseInt(n) )
			case StringLit( s ) => StringValue( s )
			case MarkLit( m ) => MarkedValue( m )
			case ValueVariableExpression( n ) =>
				columnNameMap get n.name match {
					case None => problem( n.pos, "no such column" )
					case Some( ind ) => FieldValue( ind )
				}
		}

	def evalValue( row: Vector[AnyRef], result: ValueResult ): AnyRef =
		result match {
			case NumberValue( n: Number ) => n
			case FieldValue( index: Int ) => row(index)
			case MarkedValue( m: Mark ) => m
			case StringValue( s: String ) => s
		}

	def evalCondition( row: Vector[AnyRef], cond: ConditionResult ): Boolean =
		cond match {
			case ComparisonLogical( left, pred, right ) =>
				Math( pred, evalValue(row, left), evalValue(row, right) ).asInstanceOf[Boolean]
		}

	def evalLogical( columnNameMap: Map[String, Int], ast: LogicalExpression ): ConditionResult = {
		ast match {
			case ComparisonExpression( left, List((comp, pred, right)) ) =>
				val l = evalExpression( columnNameMap, left )
				val r = evalExpression( columnNameMap, right )

				ComparisonLogical( l, pred, r )
		}
	}
}

trait ValueResult
case class NumberValue( n: Number ) extends ValueResult
case class FieldValue( index: Int ) extends ValueResult
case class MarkedValue( m: Mark ) extends ValueResult
case class StringValue( s: String ) extends ValueResult

trait ConditionResult
case class ComparisonLogical( left: ValueResult, pred: FunctionMap, right: ValueResult ) extends ConditionResult

trait StatementResult
case class InsertResult( auto: List[Map[String, AnyRef]], count: Int, created: Option[String] ) extends StatementResult
case class RelationResult( relation: Relation ) extends StatementResult