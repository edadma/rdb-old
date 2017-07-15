package xyz.hyperreal.rdb


class SelectionRelation( conn: Connection, relation: Relation, filter: LogicalResult, afuse: AggregateFunctionUseState ) extends AbstractRelation {

	def metadata = relation.metadata

	def iterator = {
		conn.aggregateCondition( relation, filter, afuse )
		relation.iterator filter (conn.evalCondition( _, filter ) == TRUE)
	}

}