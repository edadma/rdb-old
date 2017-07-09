package xyz.hyperreal.rdb


class ProjectionTupleseq( conn: Connection, relation: Relation, columns: Vector[ValueResult], afuse: AggregateFunctionUseState ) extends AbstractTupleseq {

	println( afuse)
	val header = Some( columns map (_.heading) )
	val types = columns map (_.typ)

	def iterator =
		relation.iterator map { t =>
			columns map { f =>
				conn.evalValue( t, f )
			}
		}

}