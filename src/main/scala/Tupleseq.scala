package xyz.hyperreal.rdb


trait Tupleseq extends Seq[Tuple] {

	def metadata: Metadata

}

abstract class AbstractTupleseq extends Tupleseq

