package xyz.hyperreal.rdb


case class ConcreteRelation( header: IndexedSeq[Column], body: List[Vector[AnyRef]] ) extends AbstractRelation {

	def iterator = body.iterator

}
