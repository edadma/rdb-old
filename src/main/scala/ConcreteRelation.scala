package xyz.hyperreal.rdb


class ConcreteRelation( header: IndexedSeq[Column], body: List[Tuple] ) extends AbstractRelation {

	val metadata = new Metadata( header )

	def iterator( context: List[Tuple] ) = body.iterator

}
