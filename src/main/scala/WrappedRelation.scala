package xyz.hyperreal.rdb

import collection.mutable.{ArrayBuffer, ListBuffer}


class WrappedRelation( relation: Relation, context: List[Tuple] ) extends AbstractRelation {

	val metadata = relation.metadata

	def iterator( context: List[Tuple] ) = relation.iterator( context )

	override def iterator = iterator( context )

}