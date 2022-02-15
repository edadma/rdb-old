package io.github.edadma.rdb_sjs

class ProjectionRelation(conn: Connection,
                         relation: Relation,
                         columns: Vector[ValueResult],
                         afuse: AggregateFunctionUseState)
    extends AbstractRelation {

  val metadata = new Metadata(
    columns map (c => SimpleColumn(c.table, c.heading, c.typ)))

  def iterator(context: List[Tuple]) = {
    conn.aggregateColumns(relation, columns, afuse)

    if (afuse == OnlyAFUsed)
      Iterator(conn.evalVector(null, columns))
    else
      relation.iterator(context) map (v =>
        conn.evalVector(v :: context, columns))
  }

  override def toString =
    s"project( $relation, [${columns map (_.heading) mkString ","}] )"

}
