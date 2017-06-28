package xyz.hyperreal.rdb

import collection.mutable.{ArrayBuffer, ListBuffer}


class BaseRelation( val name: String, definition: Seq[Column] ) extends AbstractRelation {

	private val cols = ArrayBuffer[Column]( definition: _* )

	private val rows = new ArrayBuffer[Vector[AnyRef]]

	def header = cols toIndexedSeq

	def iterator = rows.iterator

	def size = rows.length

	def insertRow( row: Vector[AnyRef] ) = {
		rows += row
		InsertResult( List(Map.empty), 1 )
	}

	def insertRelation( rel: Relation ) = {
		val mapping = new ArrayBuffer[AnyRef]

		for (c <- header)
			mapping += rel.columnMap.getOrElse( c, I ).asInstanceOf[AnyRef]

		val res = new ListBuffer[Map[String, AnyRef]]
		var count = 0

		for (row <- rel) {
			val r =
				(for (i <- mapping)
					yield {
						i match {
							case n: java.lang.Integer => row( n )
							case m => m
						}
					}) toVector

			insertRow( r ) match {
				case InsertResult( List( m ), c ) =>
					res += m
					count += c
			}
		}
	}
}

case class InsertResult( auto: List[Map[String, AnyRef]], count: Int )