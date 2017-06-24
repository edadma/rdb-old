package xyz.hyperreal.rdb


abstract class AbstractRelation {

	def name: String

	def columns: Seq[Column]

	def columnMap: Map[String, Int]

	def iterator: Iterator[Vector[AnyRef]]

	override def toString = name

}

case class Column( name: String, typ: String )