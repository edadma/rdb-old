package xyz.hyperreal.rdb


class InnerJoinRelation( conn: Connection, val metadata: Metadata, left: Relation, condition: LogicalResult, right: Relation ) extends AbstractRelation {

	private def nestedLoopIterator =
		(for (x <- left; y <- right) yield x ++ y).iterator filter (conn.evalCondition( _, condition ) == TRUE)

	def iterator = nestedLoopIterator

}
