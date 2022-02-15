package io.github.edadma.rdb_sjs

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}

class BaseRelation(val name: String, definition: Seq[BaseRelationColumn], baseRelations: HashMap[Symbol, BaseRelation])
    extends AbstractRelation {

  private val rows = new ArrayBuffer[Array[Any]]

  val metadata = new Metadata(definition toIndexedSeq)

  private val seqs =
    metadata.baseRelationHeader flatMap {
      case BaseRelationColumn(_, column, typ, _, _, true) => List(column -> typ.asInstanceOf[Auto].default)
      case _                                              => Nil
    } to mutable.HashMap

  private val indexes =
    metadata.baseRelationHeader map {
      case BaseRelationColumn(_, _, typ, constraint, _, auto) =>
        if (constraint.isDefined || auto)
          new mutable.TreeMap[Any, Int]()(typ)
        else
          null
    }

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

  def update(conn: Connection, cond: LogicalResult, updates: List[(Int, ValueResult)]) = {
    var count = 0

    for (i <- rows.length - 1 to 0 by -1)
      if (conn.evalCondition(List(rows(i).toIndexedSeq), cond) == TRUE) {
        for ((f, v) <- updates)
          rows(i)(f) = conn.evalValue(List(rows(i).toIndexedSeq), v)
        count += 1
      }

    count
  }

  def insertValues(columns: Seq[Ident], values: Tuple): Option[Map[String, Any]] = {
    if (columns.length != values.length)
      sys.error("insertValues: column names list not the same length as values list")

    for (c <- columns)
      if (!metadata.columnSet(c.name))
        problem(c.pos, s"insertValues: column '$c' does not exist")

    val colmap = columns map (_.name) zip values toMap
    val row =
      definition map {
        case BaseRelationColumn(_, column, typ, _, unmarkable, auto) =>
          colmap get column match {
            case Some(value) => value
            case None =>
              if (auto) {
                val v = seqs(column)

                seqs(column) = typ.asInstanceOf[Auto].next(v)
                v
              } else if (unmarkable)
                sys.error(s"insertValues: column '$column' is unmarkable")
              else
                null // handle correctly as a mark
          }
      }

    insertRow(row.toIndexedSeq)
  }

  def insertRow(row: Tuple): Option[Map[String, Any]] = {
    var auto = Map.empty[String, Any]

    for (((r, d), i) <- (row zip definition) zipWithIndex) {
//      if (r.isInstanceOf[Mark] && (d.unmarkable || d.constraint.contains(PrimaryKey)))  // todo: wasn't working
//        sys.error(s"column '${d.column}' of table '${d.table}' is unmarkable")

      d.constraint match {
        case Some(PrimaryKey) if indexes(i) contains r => return None
        case Some(Unique) if indexes(i) contains r =>
          sys.error(s"Uniqueness constraint violation: $r exists in column ${d.column}")
        case Some(ForeignKey(table, column)) =>
          if (r != null && !baseRelations(table).exists(t => t(column) == r)) { //todo: "r != null" should be checking for a mark not a null, should be done correctly
            sys.error(
              s"referential integrity violation: $r does not exist in ${table.name}(${baseRelations(table).metadata.header(column).column})")
          }
        case _ =>
      }

      if ((indexes(i) ne null) && r != null) { // todo: handle null foreign keys correctly
        indexes(i)(r) = rows.length
      }

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
                    case Some(v) => auto.next(v.asInstanceOf[Number])
                  }
                } else
                  I
            }
          }) toVector

      sys.error("insertRelation")
      insertRow(r) match {
        case None =>
        case Some(m) =>
          res += m
          count += 1
      }
    }

    (res toList, count)
  }

  def insertTupleseq(columns: Seq[Ident], data: Tupleseq): (List[Map[String, Any]], Int) = {
    val res = new ListBuffer[Map[String, Any]]
    var count = 0

    for (r <- data) {
      insertValues(columns, r) match {
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
