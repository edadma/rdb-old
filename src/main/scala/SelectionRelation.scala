package xyz.hyperreal.rdb

import java.util.NoSuchElementException


class SelectionRelation( conn: Connection, relation: Relation, condition: LogicalExpression ) extends AbstractRelation {

	def header = relation.header

	def iterator = {
		new Iterator[Vector[AnyRef]] {
			val it = relation.iterator
			var row: Vector[AnyRef] = _

			def hasNext: Boolean =
				if (row ne null)
					true
				else
					if (!it.hasNext)
						false
					else {
						row = it.next

						if (conn.evalLogical( SelectionRelation.this, row, condition ) == TRUE)
							true
						else {
							row = null
							hasNext
						}
					}

			def next = {
				if (hasNext) {
					val res = row

					row = null
					res
				} else
					throw new NoSuchElementException( "no more rows" )
			}
		}
	}

	def size = iterator.size

}