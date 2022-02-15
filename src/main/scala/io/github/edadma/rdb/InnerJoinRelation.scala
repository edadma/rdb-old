package io.github.edadma.rdb

class InnerJoinRelation(conn: Connection,
                        val metadata: Metadata,
                        left: Relation,
                        condition: LogicalResult,
                        right: Relation)
    extends AbstractRelation {

  private def nestedLoopIterator(context: List[Tuple]) =
    (for (x <- left; y <- right) yield x ++ y).iterator filter (r =>
      conn.evalCondition(r :: context, condition) == TRUE)

  def iterator(context: List[Tuple]) = nestedLoopIterator(context)

  override def toString = s"innerJoin( $left, $condition, $right )"

}
