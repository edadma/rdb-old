package xyz.hyperreal.rdb


class ProjectionTupleseq( conn: Connection, relation: Relation, columns: Vector[ValueResult] ) extends AbstractTupleseq {

	val metadata = new Metadata( columns map (c => Column("", c.toString, c.typ, None)) )

	def iterator =
		relation.iterator map { t =>
			columns map { f =>
				conn.evalValue( t, f )
			}
		}

}