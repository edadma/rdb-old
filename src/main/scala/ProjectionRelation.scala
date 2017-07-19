package xyz.hyperreal.rdb


class ProjectionRelation( conn: Connection, relation: Relation, columns: Vector[ValueResult], afuse: AggregateFunctionUseState ) extends AbstractRelation {

	val metadata = new Metadata( columns map (c => SimpleColumn( "", c.heading, c.typ )) )

	def iterator = {
		conn.aggregateColumns( relation, columns, afuse )

		if (afuse == OnlyAFUsed)
			Iterator( conn.evalVector(null, columns) )
		else
			relation.iterator map (conn.evalVector( _, columns ))
	}

}