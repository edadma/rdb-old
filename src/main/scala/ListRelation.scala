package xyz.hyperreal.rdb


class ListRelation( val header: IndexedSeq[Column], body: List[Vector[AnyRef]] ) extends AbstractRelation {

	def iterator = body.iterator

	lazy val size = body.length

}
