package xyz.hyperreal.rdb


trait Tupleseq extends Seq[Tuple] {

	def header: Option[IndexedSeq[String]]

	def types: IndexedSeq[Type]

}

abstract class AbstractTupleseq extends Tupleseq {

	def apply( idx: Int ) = iterator drop idx next

	def length = iterator length

}

