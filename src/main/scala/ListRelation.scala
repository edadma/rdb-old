package xyz.hyperreal.rdb


class ListRelation( header: IndexedSeq[Column], body: List[Vector[AnyRef]] ) extends AbstractRelation {

	val metadata = new Metadata( header )

	def iterator = body.iterator

}
