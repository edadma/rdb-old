package xyz.hyperreal.rdb_sjs

class AliasRelation(rel: Relation, alias: String) extends Relation {

  def metadata =
    new Metadata(
      rel.metadata.header map {
        case SimpleColumn(table, column, typ) =>
          SimpleColumn(alias, column, typ)
        case BaseRelationColumn(table,
                                column,
                                typ,
                                constraint,
                                unmarkable,
                                auto) =>
          BaseRelationColumn(alias, column, typ, constraint, unmarkable, auto)
      }
    )

  def collect = rel.collect

  def iterator(context: List[Tuple]) = rel.iterator(context)

  def contains(elem: Tuple) = rel.contains(elem)

  def iterator = rel.iterator

  def diff(that: collection.Set[Tuple]) = rel.diff(that)

  def boundedIterator(context: List[Tuple],
                      bounds: Seq[Symbol],
                      idx: Int,
                      v: AnyRef) = ???

}
