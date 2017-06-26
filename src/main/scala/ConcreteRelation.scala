package xyz.hyperreal.rdb


case class ConcreteRelation( header: Seq[Column], body: List[Vector[AnyRef]] ) extends AbstractRelation {

	val columnMap = (header map (_.name) zipWithIndex) toMap

	def iterator = body.iterator

}
