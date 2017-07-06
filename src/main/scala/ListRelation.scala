package xyz.hyperreal.rdb


class ListRelation( header: IndexedSeq[Column], body: List[Tuple] ) extends AbstractRelation {

	val metadata = new Metadata( header )

	def iterator = body.iterator

}
