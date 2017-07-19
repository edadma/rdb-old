package xyz.hyperreal.rdb


class GroupingRelation( conn: Connection, relation: Relation, disafuse: AggregateFunctionUseState, discriminator: Vector[ValueResult],
												dismetadata: Metadata, filtafuse: AggregateFunctionUseState, filter: Option[LogicalResult],
												colafuse: AggregateFunctionUseState, columns: Vector[ValueResult] ) extends AbstractRelation {

	val metadata = new Metadata( columns map (c => SimpleColumn( "", c.heading, c.typ )) )

	def iterator = {
		conn.aggregateColumns( relation, discriminator, disafuse )

		val groups = relation groupBy (conn.evalVector( _, discriminator ))

		groups.iterator filter {
			case (k, g) if filter nonEmpty =>
				conn.aggregateCondition( g, filter get, filtafuse )
				conn.evalCondition( k, filter get ) == TRUE
			case _ => true
		} map { case (k, g) =>
			conn.aggregateColumns( g, columns, colafuse )
			conn.evalVector( k, columns )
		}
	}
}