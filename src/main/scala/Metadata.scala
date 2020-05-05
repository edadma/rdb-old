package xyz.hyperreal.rdb_sjs

class Metadata(val header: IndexedSeq[Column]) {

  lazy val baseRelationHeader =
    header.asInstanceOf[IndexedSeq[BaseRelationColumn]]

  lazy val tableSet = header map (_.table) toSet

  lazy val columnSet = header map (_.column) toSet

  lazy val columnMap = (header map (_.column) zipWithIndex) toMap

  lazy val tableColumnMap =
    (header map { case Column(t, c, _) => (t, c) } zipWithIndex) toMap

  lazy val attributes = header map { case Column(_, n, t) => (n, t) } toSet

  lazy val primaryKey = baseRelationHeader find (_.constraint contains PrimaryKey)

  lazy val primaryKeyIndex = columnMap(primaryKey.get.column)

  override def toString = header.toString
}

object Column {

  def unapply(c: Column) = Some((c.table, c.column, c.typ))

}

abstract class Column {

  def table: String
  def column: String
  def typ: Type

}

case class SimpleColumn(table: String, column: String, typ: Type) extends Column
case class BaseRelationColumn(table: String,
                              column: String,
                              typ: Type,
                              constraint: Option[Constraint],
                              unmarkable: Boolean,
                              auto: Boolean)
    extends Column

trait Constraint
case object PrimaryKey extends Constraint
case class ForeignKey(table: BaseRelation, column: Int) extends Constraint
case object Unique extends Constraint
case object Indexed extends Constraint
