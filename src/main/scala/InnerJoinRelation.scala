package xyz.hyperreal.rdb


class InnerJoinRelation( conn: Connection, val metadata: Metadata, left: Relation, condition: ConditionResult, right: Relation ) extends AbstractRelation {

	private def nextLoopIterator =
		(for (x <- left; y <- right) yield x ++ y).iterator filter (conn.evalCondition( _, condition ))

	def iterator = nextLoopIterator

}
