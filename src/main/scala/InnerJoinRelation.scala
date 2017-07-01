package xyz.hyperreal.rdb


class InnerJoinRelation( conn: Connection, left: Relation, condition: ConditionResult, right: Relation ) extends AbstractRelation {

	def header = left.header ++ right.header

	private def nextLoopIterator =
		(for (x <- left; y <- right) yield x ++ y).iterator filter (conn.evalCondition( _, condition ))

	def iterator = nextLoopIterator

}
