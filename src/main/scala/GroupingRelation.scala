package xyz.hyperreal.rdb_sjs


class GroupingRelation( conn: Connection, relation: Relation, disafuse: AggregateFunctionUseState, discriminator: Vector[ValueResult],
												filtafuse: AggregateFunctionUseState, filter: Option[LogicalResult],
												colafuse: AggregateFunctionUseState, columns: Vector[ValueResult] ) extends AbstractRelation {

	val metadata = new Metadata( columns map (c => SimpleColumn( "", c.heading, c.typ )) )

	def iterator( context: List[Tuple] ) = {
		conn.aggregateColumns( relation, discriminator, disafuse )

		val groups = relation groupBy (v => conn.evalVector( v :: context, discriminator ))

		groups.iterator filter {
			case (k, g) if filter nonEmpty =>
				conn.aggregateCondition( g, filter get, filtafuse )
				conn.evalCondition( k :: context, filter get ) == TRUE
			case _ => true
		} map { case (k, g) =>
			conn.aggregateColumns( g, columns, colafuse )
			conn.evalVector( k :: context, columns )
		}
	}

	override def toString = s"group( $relation, $discriminator, $columns, $filter )"

}