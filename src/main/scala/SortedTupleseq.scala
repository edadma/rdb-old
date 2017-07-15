package xyz.hyperreal.rdb

import collection.mutable.ArrayBuffer


class SortedTupleseq( relation: Relation, fields: List[Int], ascending: Boolean ) extends AbstractTupleseq {

	val header = Some( relation.metadata.header map (_.column) )

	val types = relation.metadata.header map (_.typ)

	def lt( left: Tuple, right: Tuple ) = {
		def compare( fields: List[Int] ): Boolean =
			fields match {
				case Nil => false
				case f :: tail =>
					types(f).compare( left(f), right(f) ) match {
						case -1 => ascending
						case 0 => compare( tail )
						case 1 => !ascending
					}
			}

		compare( fields )
	}

	def iterator = {
		val rows = new ArrayBuffer[Tuple]

		rows ++= relation
		rows.sortWith( lt ).iterator
	}

}
