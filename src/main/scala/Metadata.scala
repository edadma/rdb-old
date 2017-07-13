package xyz.hyperreal.rdb


class Metadata( val header: IndexedSeq[Column] ) {

	lazy val tableSet = header map (_.table) toSet

	lazy val columnSet = header map (_.column) toSet

	lazy val columnMap = (header map (_.column) zipWithIndex) toMap

	lazy val tableColumnMap = (header map {case Column(t, c, _, _) => (t, c)} zipWithIndex) toMap

	lazy val attributes = header map {case Column(_, n, t, _) => (n, t)} toSet

	lazy val primaryKey = header find (_.constraint contains PrimaryKey)

	lazy val primaryKeyIndex = columnMap(primaryKey.get.column)

}

case class Column( table: String, column: String, typ: Type, constraint: Option[Constraint] )

trait Constraint
case object PrimaryKey extends Constraint
case class ForeignKey( table: String, column: String ) extends Constraint
case object Unique extends Constraint
case object Indexed extends Constraint