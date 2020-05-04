package xyz.hyperreal.rdb_sjs


trait Relation extends collection.Set[Tuple] {

	def metadata: Metadata

	def collect: Relation

	def iterator( context: List[Tuple] ): Iterator[Tuple]

	def iteratorEq( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorNe( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorLt( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorLte( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorGlt( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorGlte( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorGt( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorGte( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorLgt( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]

	def iteratorLgte( context: List[Tuple], idx: Int, v: AnyRef ): Iterator[Tuple]
}

abstract class AbstractRelation extends Relation {

//	def -( elem: Tuple ) = sys.error( "unsupported" )
//
//	def +( elem: Tuple ) = sys.error( "unsupported" )

	def contains( elem: Tuple ) = iterator contains elem	// extending classes can override with a more effecient implementation

	def diff( that: collection.Set[Tuple] ) = filterNot (that contains)	// extending classes can override with a more effecient implementation

	def collect = new ConcreteRelation( metadata.header, iterator.toList )

	def iterator = iterator( Nil )

	def iteratorEq( context: List[Tuple], idx: Int, v: AnyRef ) = iterator( context ) filter (_(idx) == v)

	def iteratorNe( context: List[Tuple], idx: Int, v: AnyRef ) = iterator( context ) filter (_(idx) != v)

	def iteratorLt( context: List[Tuple], idx: Int, v: AnyRef ) = ns

	def iteratorLte( context: List[Tuple], idx: Int, v: AnyRef ) = ns

	def iteratorGlt( context: List[Tuple], idx: Int, v: AnyRef ) = na( "G<" )

	def iteratorGlte( context: List[Tuple], idx: Int, v: AnyRef ) = na( "G<=" )

	def iteratorGt( context: List[Tuple], idx: Int, v: AnyRef ) = ns

	def iteratorGte( context: List[Tuple], idx: Int, v: AnyRef ) = ns

	def iteratorLgt( context: List[Tuple], idx: Int, v: AnyRef ) = na( "L>" )

	def iteratorLgte( context: List[Tuple], idx: Int, v: AnyRef ) = na( "L>=" )

	def ns = sys.error( "not supported yet, working on it" )

	def na( comp: String ) = sys.error( s"comparison '$comp' can only be use with a base relation column that is indexed" )
}
