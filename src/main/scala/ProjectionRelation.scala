package xyz.hyperreal.rdb


class ProjectionRelation( relation: Relation, columns: List[String] ) extends AbstractRelation {

	require( columns.toSet.size == columns.length, "columns contains duplicate" )

	val ind = columns map relation.columnMap toVector
	val header = ind map relation.header

	def iterator =
		new Iterator[Vector[AnyRef]] {
			val it = relation.iterator

			def hasNext = it.hasNext

			def next = {
				val r = it.next

				ind map r
			}
		}

	override def size = relation.size

}