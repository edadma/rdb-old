package xyz.hyperreal.rdb_sjs

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
      var mismatch = false

      for (y <- right) {
        val r = x ++ y

        if (conn.evalCondition(r :: context, condition) == TRUE)
          buf += r
        else {
          if (!mismatch) {
            buf += x ++ IndexedSeq.fill(y.length)(null)
            mismatch = true
          }
        }
      }
    }

    buf.iterator
  }

  def iterator(context: List[Tuple]) = nestedLoopIterator(context)

  override def toString = s"innerJoin( $left, $condition, $right )"

}
