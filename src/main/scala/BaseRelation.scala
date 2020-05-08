package xyz.hyperreal.rdb_sjs

import collection.mutable.{ArrayBuffer, ListBuffer, TreeMap}

class BaseRelation(val name: String, definition: Seq[BaseRelationColumn])
    extends AbstractRelation {

  private val rows = new ArrayBuffer[Array[Any]]

  val metadata = new Metadata(definition toIndexedSeq)

  private val indexes =
    metadata.baseRelationHeader map {
      case BaseRelationColumn(_, _, typ, constraint, _, auto) =>
        if (constraint.isDefined || auto)
          new TreeMap[Any, Int]()(typ)
        else
          null
    }

//	private val pkindex =
//		metadata primaryKey match {
//			case None => sys.error( s"attempting to create base relation '$name' with no primary key" )
//			case Some( BaseRelationColumn( _, col, typ, _ ) ) =>
//				val index = new TreeMap[Any, Int]()( typ )
//
//				indexes(col) = index
//				index
//		}

  def iterator(context: List[Tuple]) = rows.iterator map (_ toVector)

  override def size = rows.length

  def delete(conn: Connection, cond: LogicalResult) = {
    var count = 0

    for (i <- rows.length - 1 to 0 by -1)
      if (conn.evalCondition(List(rows(i).toIndexedSeq), cond) == TRUE) {
        rows.remove(i)
        count += 1
      }

    count
  }

  def update(conn: Connection,
             cond: LogicalResult,
             updates: List[(Int, ValueResult)]) = {
    var count = 0

    for (i <- rows.length - 1 to 0 by -1)
      if (conn.evalCondition(List(rows(i).toIndexedSeq), cond) == TRUE) {
        for ((f, v) <- updates)
          rows(i)(f) = conn.evalValue(List(rows(i).toIndexedSeq), v)
        count += 1
      }

    count
  }

  def insertRow(row: Tuple): Option[Map[String, Any]] = {
    var auto = Map.empty[String, Any]

    for (((r, d), i) <- (row zip definition) zipWithIndex) {
      if (r.isInstanceOf[Mark] && (d.unmarkable || d.constraint.contains(
            PrimaryKey)))
        sys.error(s"column '${d.column}' of table '${d.table}' is unmarkable")

      d.constraint match {
        case Some(PrimaryKey) if indexes(i) contains r => return None
        case Some(Unique) if indexes(i) contains r =>
          sys.error(
            s"Uniqueness constraint violation: $r exists in column ${d.column}")
        case Some(ForeignKey(table, column)) =>
          if (!table.exists(t => t(column) == r))
            sys.error(
              s"referential integrity violation: $r does not exist in ${table.name}(${table.metadata.header(column).column})")
        case _ =>
      }

      if (indexes(i) ne null)
        indexes(i)(r) = rows.length

      if (d.auto)
        auto += (d.column -> r)
    }

    rows += row.toArray
    Some(auto)
  }

  def insertRelation(rel: Relation) = {
    val mapping = new ArrayBuffer[Option[Int]]

    for (c <- metadata.header)
      mapping += rel.metadata.columnMap get c.column

    val res = new ListBuffer[Map[String, Any]]
    var count = 0

    for (row <- rel) {
      val r =
        (for ((m, idx) <- mapping zipWithIndex)
          yield {
            m match {
              case Some(n) => row(n)
              case None =>
                if (metadata.baseRelationHeader(idx).auto) {
                  val auto =
                    metadata.baseRelationHeader(idx).typ.asInstanceOf[Auto]

                  indexes(idx).lastOption map (_._1) match {
                    case None    => auto.default
                    case Some(v) => auto.next(v)
                  }
                } else
                  I
            }
          }) toVector

      insertRow(r) match {
        case None =>
        case Some(m) =>
          res += m
          count += 1
      }
    }

    (res toList, count)
  }

  def insertTupleseq(data: Tupleseq) = {
    val res = new ListBuffer[Map[String, Any]]
    var count = 0

    for (r <- data) {
      insertRow(r) match {
        case None =>
        case Some(m) =>
          res += m
          count += 1
      }
    }

    (res toList, count)
  }

  override def toString = s"baseRelation( $name, $definition )"
}
