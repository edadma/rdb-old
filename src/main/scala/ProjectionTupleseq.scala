package xyz.hyperreal.rdb


class ProjectionTupleseq( conn: Connection, relation: Relation, columns: Vector[ValueResult], afuse: AggregateFunctionUseState ) extends AbstractTupleseq {

	val header = Some( columns map (_.heading) )
	val types = columns map (_.typ)

	def iterator = {
		if (afuse == AFUsed) {
			for (t <- relation.iterator)
				columns map (conn.evalValue( t, _ ))

			Iterator( columns map (conn.evalValue( null, _ )) )
		} else
			relation.iterator map { t =>
				columns map (conn.evalValue( t, _ ))
			}
	}

}