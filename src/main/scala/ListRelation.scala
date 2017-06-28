package xyz.hyperreal.rdb


case class ListRelation( header: IndexedSeq[Column], body: List[Vector[AnyRef]] ) extends AbstractRelation {

	def iterator = body.iterator

	lazy val size = body.length

}
