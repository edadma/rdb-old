package xyz.hyperreal.rdb

import java.util.NoSuchElementException


class SelectionRelation( conn: Connection, relation: Relation, condition: ConditionResult ) extends AbstractRelation {

	def metadata = relation.metadata

	def iterator = relation.iterator filter (conn.evalCondition( _, condition ))

//	def iterator = {
//		new Iterator[Vector[AnyRef]] {
//			val it = relation.iterator
//			var row: Vector[AnyRef] = _
//
//			def hasNext: Boolean =
//				if (row ne null)
//					true
//				else
//					if (!it.hasNext)
//						false
//					else {
//						row = it.next
//
//						if (conn.evalCondition( row, condition ))
//							true
//						else {
//							row = null
//							hasNext
//						}
//					}
//
//			def next = {
//				if (hasNext) {
//					val res = row
//
//					row = null
//					res
//				} else
//					throw new NoSuchElementException( "no more rows" )
//			}
//		}
//	}

}