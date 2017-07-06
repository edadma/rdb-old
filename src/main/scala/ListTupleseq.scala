package xyz.hyperreal.rdb


class ListTupleseq( header: IndexedSeq[Type], body: List[Tuple] ) extends AbstractTupleseq {

	val metadata = new Metadata( header map (Column( "", anonymous, _, None )) )

	def apply( idx: Int ) = body.apply( idx )

	def iterator = body.iterator

	def length = body.length

}
