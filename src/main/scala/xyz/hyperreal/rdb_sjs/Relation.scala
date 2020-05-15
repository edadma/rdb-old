package xyz.hyperreal.rdb_sjs

trait Relation extends collection.Set[Tuple] {

  def metadata: Metadata

  def collect: Relation

  def iterator(context: List[Tuple]): Iterator[Tuple]

  def sort(conn: Connection, exprs: List[(ValueResult, Int)]): Relation

  def boundedIterator(context: List[Tuple],
                      bounds: Seq[Symbol],
                      idx: Int,
                      v: AnyRef): Iterator[Tuple]

}

abstract class AbstractRelation extends Relation {

  def contains(elem: Tuple) =
    iterator contains elem // extending classes can override with a more effecient implementation

  def diff(that: collection.Set[Tuple]) =
    filterNot(that contains) // extending classes can override with a more effecient implementation

  def collect = new ConcreteRelation(metadata.header, iterator.toList)

  // todo: sort doesn't take into account context
  def sort(conn: Connection, exprs: List[(ValueResult, Int)]) = {
    val data = iterator.toList

    def compare(exprs: List[(ValueResult, Int)], a: Tuple, b: Tuple): Int =
      exprs match {
        case Nil => 0
        case (v, d) :: tail =>
          val comp =
            d * ((conn.evalValue(List(a), v), conn.evalValue(List(b), v)) match {
              case (x: String, y: String) => x compare y
              case (x: Int, y: Int)       => x compare y
              case (x: Double, y: Double) => x compare y
            })

          if (comp != 0)
            comp
          else
            compare(tail, a, b)
      }

    def lt(a: Tuple, b: Tuple) = compare(exprs, a, b) < 0

    new ConcreteRelation(metadata.header, data sortWith lt)
  }

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
