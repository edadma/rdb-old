package xyz.hyperreal.rdb


class GroupingRelation( conn: Connection, relation: Relation, disafuse: AggregateFunctionUseState, discriminator: List[ValueResult],
												colafuse: AggregateFunctionUseState, columns: List[ValueResult] ) extends AbstractRelation {

	val metadata = new Metadata( columns map (c => Column( "", c.heading, c.typ, None )) toVector )

	def iterator = {
		val groups = relation.groupBy( t => discriminator map (d => conn.evalValue(t, d)) )


	}
}