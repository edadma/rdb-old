package xyz.hyperreal.rdb_sjs

trait Relation extends collection.Set[Tuple] {

  def metadata: Metadata

  def collect: Relation

  def iterator(context: List[Tuple]): Iterator[Tuple]

  def boundedIterator(context: List[Tuple],
                      bounds: Seq[Symbol],
                      idx: Int,
                      v: AnyRef): Iterator[Tuple]
}

abstract class AbstractRelation extends Relation {

//	def -( elem: Tuple ) = sys.error( "unsupported" )
//
//	def +( elem: Tuple ) = sys.error( "unsupported" )

  def contains(elem: Tuple) =
    iterator contains elem // extending classes can override with a more effecient implementation

  def diff(that: collection.Set[Tuple]) =
    filterNot(that contains) // extending classes can override with a more effecient implementation

  def collect = new ConcreteRelation(metadata.header, iterator.toList)

  def iterator = iterator(Nil)

  def boundedIterator(context: List[Tuple],
                      bounds: Seq[Symbol],
                      idx: Int,
                      v: AnyRef) = ns

  def ns = sys.error("not supported yet")

  def na(comp: String) =
    sys.error(
      s"comparison '$comp' can only be use with a base relation column that is indexed")
}
