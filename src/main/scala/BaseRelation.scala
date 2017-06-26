package xyz.hyperreal.rdb

import collection.mutable.ArrayBuffer


class BaseRelation( val name: String, definition: List[Column] ) extends AbstractRelation {

	private val cols = ArrayBuffer[Column]( definition: _* )

	private val rows = new ArrayBuffer[Vector[AnyRef]]

	def header = cols toList

	def columnMap = (cols map (_.name) zipWithIndex) toMap

	def iterator = rows.iterator

	def insert( row: Vector[AnyRef] ): Unit = {
		rows += row
	}

	def insertRelation( rel: AbstractRelation ): Unit = {
		for (row <- rel)
			insert( row )
	}
}
