package io.github.edadma.rdb

class AliasRelation(rel: Relation, alias: String) extends AbstractRelation {

  def metadata =
    new Metadata(
      rel.metadata.header map {
        case SimpleColumn(_, column, typ) =>
          SimpleColumn(alias, column, typ)
        case BaseRelationColumn(_, column, typ, constraint, unmarkable, auto) =>
          BaseRelationColumn(alias, column, typ, constraint, unmarkable, auto)
      }
    )

  def iterator(context: List[Tuple]) = rel.iterator(context)

}
