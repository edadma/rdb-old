package xyz.hyperreal.rdb


class ListTupleseq( header: IndexedSeq[Type], body: List[Tuple] ) extends AbstractTupleseq {

	val metadata = new Metadata( header map (Column( "", anonymous, _, None )) )

	def iterator = body.iterator

}
