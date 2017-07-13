package xyz.hyperreal.rdb


class GroupingRelation( conn: Connection, relation: Relation, disafuse: AggregateFunctionUseState, discriminator: Vector[ValueResult],
												dismetadata: Metadata, colafuse: AggregateFunctionUseState, columns: Vector[ValueResult] ) extends AbstractRelation {

	val metadata = new Metadata( columns map (c => Column( "", c.heading, c.typ, None )) )

	def iterator = {
		conn.aggregateColumns( relation, discriminator, disafuse )

		val groups = relation groupBy (conn.evalVector( _, discriminator ))

		for ((k, g) <- groups.iterator)
			yield {
				conn.aggregateColumns( g, columns, colafuse )
				conn.evalVector( k, columns )
			}
	}
}