package xyz.hyperreal.rdb


trait Relation extends collection.Set[Tuple] {

	def metadata: Metadata

	def collect = new ListRelation( metadata.header, iterator.toList )

}

abstract class AbstractRelation extends Relation {

	def -( elem: Tuple ) = sys.error( "unsupported" )

	def +( elem: Tuple ) = sys.error( "unsupported" )

	def contains( elem: Tuple ) = iterator contains elem	// extending classes can override with a more effecient implementation
}
