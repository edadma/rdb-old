package xyz.hyperreal.rdb_sjs


class ConcreteTupleseq( val types: IndexedSeq[Type], body: List[Tuple] ) extends AbstractTupleseq {

	val header = None

	def iterator = body.iterator

}
