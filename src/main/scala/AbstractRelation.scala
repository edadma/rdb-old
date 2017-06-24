package xyz.hyperreal.rdb


abstract class AbstractRelation {

	def name: String

	def columns: Seq[Column]

	def columnMap: Map[String, Int]

	def iterator: Iterator[Vector[AnyRef]]

	def foreach( f: Vector[AnyRef] => Unit ): Unit = {
		for (row <- iterator)
			f( row )
	}

	override def toString = name

}

case class Column( name: String, typ: String )