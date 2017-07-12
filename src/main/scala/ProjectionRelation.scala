package xyz.hyperreal.rdb


class ProjectionRelation( conn: Connection, relation: Relation, columns: Vector[ValueResult], afuse: AggregateFunctionUseState ) extends AbstractTupleseq {

	val header = Some( columns map (_.heading) )
	val types = columns map (_.typ)

	def iterator = {
		if (afuse == OnlyAFUsed || afuse == FieldAndAFUsed) {
			for (c <- columns)
				conn.initAggregation( c )

			for (t <- relation.iterator; c <- columns)
				conn.aggregate( t, c )
		}

		if (afuse == OnlyAFUsed)
			Iterator( columns map (conn.evalValue( null, _ )) )
		else
			relation.iterator map { t =>
				columns map (conn.evalValue( t, _ ))
			}
	}

}