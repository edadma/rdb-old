package xyz.hyperreal.rdb


case class Relation( name: String, columns: Seq[Column], rows: List[Vector[AnyRef]] ) extends AbstractRelation {

	val columnMap = (columns map (_.name) zipWithIndex) toMap

	def iterator = rows.iterator

}
