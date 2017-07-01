package xyz.hyperreal.rdb

import collection.mutable.{ArrayBuffer, ListBuffer}


class BaseRelation( name: String, definition: Seq[Column] ) extends AbstractRelation {

	private val cols = ArrayBuffer[Column]( definition map {case Column( _, col, typ ) => Column( name, col, typ )}: _* )

	private val rows = new ArrayBuffer[Vector[AnyRef]]

	def header = cols toIndexedSeq

	def iterator = rows.iterator

	override def size = rows.length

	private [rdb] def insertRow( row: Vector[AnyRef] ): Option[Map[String, AnyRef]] = {
		rows += row
		Some( Map.empty )
	}

	private [rdb] def insertRelation( rel: Relation ) = {
		val mapping = new ArrayBuffer[AnyRef]

		for (c <- header)
			mapping += rel.columnMap.getOrElse( c.column, I ).asInstanceOf[AnyRef]

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
				case None =>
				case Some( m ) =>
					res += m
					count += 1
			}
		}

		(res toList, count)
	}

	private [rdb] def insertTupleset( data: List[Vector[AnyRef]] ) = {
		val res = new ListBuffer[Map[String, AnyRef]]
		var count = 0

		for (r <- data) {
			insertRow( r ) match {
				case None =>
				case Some( m ) =>
					res += m
					count += 1
			}
		}

		(res toList, count)
	}
}
