package xyz.hyperreal.rdb


class SelectionRelation( relation: Relation, condition: LogicalExpression ) extends AbstractRelation {

	def header = relation.header

	def iterator = {
		new Iterator[Vector[AnyRef]] {
			def hasNext = true

			def next = {
				null
			}
		}
	}

	def size = iterator.size

}