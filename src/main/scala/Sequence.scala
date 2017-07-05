package xyz.hyperreal.rdb


trait Sequence extends Seq[Tuple] {

	def metadata: Metadata

}

abstract class AbstractSequence extends Sequence

