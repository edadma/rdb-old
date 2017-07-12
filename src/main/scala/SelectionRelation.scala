package xyz.hyperreal.rdb


class SelectionRelation( conn: Connection, relation: Relation, condition: LogicalResult, afuse: AggregateFunctionUseState ) extends AbstractRelation {

	def metadata = relation.metadata

	def iterator = {
		conn.aggregateCondition( relation, condition, afuse )
		relation.iterator filter (conn.evalCondition( _, condition ) == TRUE)
	}

}