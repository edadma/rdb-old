package xyz.hyperreal.rdb


trait Relation extends Iterable[Tuple] {

	def metadata: Metadata

	def collect = new ListRelation( metadata.header, iterator.toList )

}

abstract class AbstractRelation extends Relation
