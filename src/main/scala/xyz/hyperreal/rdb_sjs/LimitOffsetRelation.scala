package xyz.hyperreal.rdb_sjs

class LimitOffsetRelation(relation: Relation, limit: Option[Int], offset: Option[Int]) extends AbstractRelation {

  val metadata = relation.metadata

  def iterator(context: List[Tuple]) =
    relation.iterator(context) drop offset.getOrElse(0) take limit.getOrElse(Int.MaxValue)

  override def toString = s"limitOffset( $relation, $limit, $offset )"
}
