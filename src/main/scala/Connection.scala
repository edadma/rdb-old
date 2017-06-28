package xyz.hyperreal.rdb

import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, ListBuffer}


class Connection {

	val baseRelations = new HashMap[String, BaseRelation]

	def evalRelation( ast: RelationExpression ): Relation = {
		ast match {
			case VariableRelationExpression( Ident(p, n) ) =>

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
			case LiteralRelationExpression( columns, data ) =>
				var hset = Set[String]()

				for (ColumnSpec( Ident(p, n), _, _ ) <- columns)
					if (hset(n))
						problem( p, s"duplicate $n" )
					else
						hset += n

				val body = new ArrayBuffer[Vector[AnyRef]]
				val types =
					columns map {
						case ColumnSpec( _, _, None ) => null
						case ColumnSpec( _, p, Some(t) ) => Type.fromSpec( p, t )
					} toArray

				if (data isEmpty)
					types indexOf null match {
						case -1 =>
						case ind => problem( columns(ind).typepos, "missing type specification in empty relation" )
					}
				else {
					for (r <- data) {
						val row = new ArrayBuffer[AnyRef]

						if (r.length < types.length)
							problem( r.last.pos, "unexpected last value (row too short)")

						for ((v, i) <- r zipWithIndex) {
							if (i == types.length)
								problem( v.pos, "too many values")

							var x = evalExpression( v )

							val y = x match {
								case _: Mark =>
								case a: java.lang.Integer =>
									types(i) match {
										case null => types(i) = IntegerType
										case IntegerType =>
										case FloatType =>
											x = a.toDouble.asInstanceOf[Number]
										case t => problem( v.pos, s"expected $t, not integer" )
									}
								case a: String =>
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
				}

				val header =
					columns zip types map {
						case (ColumnSpec( _, p, _ ), null) => problem( p, "missing type specification in relation with missing values" )
						case (ColumnSpec( Ident(_, n), _ , _), t) => Column( n, t )
					}

				ListRelation( header toIndexedSeq, body toList )
		}
	}

	def evalExpression( ast: ValueExpression ): AnyRef = {
		ast match {
			case NumberLit( n ) => n.toDouble.asInstanceOf[Number]
			case IntegerLit( n ) => n.toInt.asInstanceOf[Number]
			case StringLit( s ) => s
			case MarkLit( m ) => m
		}
	}

	def evalLogical() {}
}