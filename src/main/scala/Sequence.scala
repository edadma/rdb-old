package xyz.hyperreal.rdb


trait Sequence extends Seq[Vector[AnyRef]] {

	def metadata: Metadata

}

abstract class AbstractSequence extends Sequence

