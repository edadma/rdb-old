package xyz.hyperreal.rdb


class SelectionRelation( relation: Relation, condition: LogicalExpression ) extends AbstractRelation {

	def header = relation.header

	def iterator = {
		new Iterator[Vector[AnyRef]] {
			val it = relation.iterator
			var row: Vector[AnyRef] = _

			def hasNext = {
				if (row ne null)
					true
				else {
					if (!it.hasNext)
						false
					else {
						row = it.next
					}
				}
			}

			def next = {
				null
			}
		}
	}

	def size = iterator.size

}