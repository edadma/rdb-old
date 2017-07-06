package xyz.hyperreal.rdb

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}

import xyz.hyperreal.lia.{FunctionMap, Math}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]
	val varRelations = new HashMap[String, Relation]

	def executeStatement( statement: String ): StatementResult = {
		val p = new RQLParser
		val ast = p.parseFromString( statement, p.statement )

		ast match {
			case AssignRelationStatement( Ident(pos, name), relation ) =>
				if (baseRelations contains name)
					problem( pos, "a base relation by that name already exists" )

				val rel = evalRelation( relation )
				val res = AssignResult( name, varRelations contains name, rel size )

				varRelations(name) = rel
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
							if (varRelations contains name)
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
						val cond = evalLogical( b.metadata, condition )

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
				val types1 =
					if (types eq null)
						new Array[Type]( columns.length )
					else
						types
				val rel = evalRelation( relation )


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
				val rrel = evalRelation( right )
				val metadata = new Metadata( lrel.metadata.header ++ rrel.metadata.header )

				new InnerJoinRelation( this, metadata, lrel, evalLogical(metadata, condition), rrel )
			case SelectionRelationExpression( relation, condition ) =>
				val rel = evalRelation( relation )
				val cond = evalLogical( rel.metadata, condition )

				new SelectionRelation( this, rel, cond )
			case RelationVariableExpression( Ident(p, n) ) =>
				baseRelations get n match {
					case None =>
						varRelations get n match {
							case None => problem( p, "unknown base or variable relation" )
							case Some( r ) => r
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
						case ColumnSpec( _, p, Some(t), _, _, _ ) => Type.fromSpec( p, t )
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

	def evalExpression( metadata: Metadata, ast: ValueExpression ): ValueResult =
		ast match {
			case FloatLit( n ) => NumberValue( java.lang.Double.parseDouble(n) )
			case IntegerLit( n ) => NumberValue( Integer.parseInt(n) )
			case StringLit( s ) => StringValue( s )
			case MarkLit( m ) => MarkedValue( m )
			case ValueVariableExpression( n ) =>
				metadata.columnMap get n.name match {
					case None => problem( n.pos, "no such column" )
					case Some( ind ) => FieldValue( ind )
				}
			case ValueColumnExpression( t, c ) =>
				if (!metadata.tableSet(t.name))
					problem( t.pos, "unknown table" )
				else
					metadata.tableColumnMap get (t.name, c.name) match {
						case None => problem( c.pos, "no such column" )
						case Some( ind ) => FieldValue( ind )
					}
		}

	def evalValue( row: Tuple, result: ValueResult ): AnyRef =
		result match {
			case NumberValue( n: Number ) => n
			case FieldValue( index: Int ) => row(index)
			case MarkedValue( m: Mark ) => m
			case StringValue( s: String ) => s
		}

	def evalCondition( row: Tuple, cond: ConditionResult ): Boolean =
		cond match {
			case ComparisonLogical( left, pred, right ) =>
				Math( pred, evalValue(row, left), evalValue(row, right) ).asInstanceOf[Boolean]
		}

	def evalLogical( metadata: Metadata, ast: LogicalExpression ): ConditionResult = {
		ast match {
			case ComparisonExpression( left, List((_, pred, right)) ) =>
				val l = evalExpression( metadata, left )
				val r = evalExpression( metadata, right )

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
case class AssignResult( name: String, update: Boolean, count: Int ) extends StatementResult
case class InsertResult( auto: List[Map[String, AnyRef]], count: Int, created: Option[String] ) extends StatementResult
case class DeleteResult( count: Int ) extends StatementResult
case class RelationResult( relation: Relation ) extends StatementResult
case class TupleseqResult( tupleseq: Tupleseq ) extends StatementResult