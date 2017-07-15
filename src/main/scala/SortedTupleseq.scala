package xyz.hyperreal.rdb

import collection.mutable.ArrayBuffer


class SortedTupleseq( relation: Relation, fields: List[Int], ascending: Boolean ) extends AbstractTupleseq {

	val header = Some( relation.metadata.header map (_.column) )

	val types = relation.metadata.header map (_.typ)

	def lt( left: Tuple, right: Tuple ): Boolean = {
		for (f <- fields)
			if (types(f).lt( left(f), right(f) ))
				return true

		false
	}

	def iterator = {
		val rows = new ArrayBuffer[Tuple]

		rows ++= relation
		rows.sortWith( lt ).iterator
	}

}
