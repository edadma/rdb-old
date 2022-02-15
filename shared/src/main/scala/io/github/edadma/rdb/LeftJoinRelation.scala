package io.github.edadma.rdb

import scala.collection.mutable.ListBuffer

class LeftJoinRelation(conn: Connection,
                       val metadata: Metadata,
                       left: Relation,
                       condition: LogicalResult,
                       right: Relation)
    extends AbstractRelation {

  private def nestedLoopIterator(context: List[Tuple]) = {
    val buf = new ListBuffer[Tuple]

    for (x <- left) {
      var matched = false

      for (y <- right) {
        val r = x ++ y

        if (conn.evalCondition(r :: context, condition) == TRUE) {
          buf += r
          matched = true
        }
      }

      if (!matched)
        buf += x ++ IndexedSeq.fill(right.metadata.header.length)(null)
    }

    buf.iterator
  }

  def iterator(context: List[Tuple]) = nestedLoopIterator(context)

  override def toString = s"leftJoin( $left, $condition, $right )"

}
