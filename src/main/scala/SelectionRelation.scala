package xyz.hyperreal.rdb


class SelectionRelation( conn: Connection, relation: Relation, filter: LogicalResult, afuse: AggregateFunctionUseState ) extends AbstractRelation {

	def metadata = relation.metadata

	def iterator( context: List[Tuple] ) = {
		conn.aggregateCondition( relation, filter, afuse )
		relation.iterator( context ) filter (r => conn.evalCondition( r :: context, filter ) == TRUE)
	}

}