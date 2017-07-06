package xyz.hyperreal.rdb

import collection.mutable.{ArrayBuffer, ListBuffer, HashMap, TreeMap}


class BaseRelation( name: String, definition: Seq[Column] ) extends AbstractRelation {

	private val cols = ArrayBuffer[Column]( definition map {case Column( _, col, typ, pk ) => Column( name, col, typ, pk )}: _* )

	private val rows = new ArrayBuffer[Tuple]

	val metadata = new Metadata( cols toIndexedSeq )

	private val indexes = new HashMap[String, TreeMap[AnyRef, Int]]
	private val pkindex =
		metadata primaryKey match {
			case None => sys.error( s"attempt to create base relation '$name' with no primary key" )
			case Some( Column( _, col, typ, _ ) ) =>
				val index = new TreeMap[AnyRef, Int]()( typ )

				indexes(col) = index
				index
		}

	def iterator = rows.iterator

	override def size = rows.length

	private [rdb] def delete( conn: Connection, cond: ConditionResult ) = {
		var count = 0

		for (i <- rows.length - 1 to 0 by -1)
			if (conn.evalCondition( rows(i), cond )) {
				rows.remove( i )
				count += 1
			}

		count
	}

	private [rdb] def insertRow( row: Tuple ): Option[Map[String, AnyRef]] = {
		rows += row
		Some( Map.empty )
	}

	private [rdb] def insertRelation( rel: Relation ) = {
		val mapping = new ArrayBuffer[AnyRef]

		for (c <- metadata.header)
			mapping += rel.metadata.columnMap.getOrElse( c.column, I ).asInstanceOf[AnyRef]

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

	private [rdb] def insertTupleseq( data: Tupleseq ) = {
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
