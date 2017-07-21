package xyz.hyperreal.rdb


class SelectionRelation( conn: Connection, relation: Relation, filter: LogicalResult, afuse: AggregateFunctionUseState ) extends AbstractRelation {

	val metadata = relation.metadata

	def iterator( context: List[Tuple] ) = {
		println( relation, filter )
		conn.aggregateCondition( relation, filter, afuse )
		relation.iterator( context ) filter (r => conn.evalCondition( r :: context, filter ) == TRUE)
	}

	override def toString = s"select( $relation, ${filter.heading} )"
}