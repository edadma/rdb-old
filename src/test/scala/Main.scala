package xyz.hyperreal.rdb

import xyz.hyperreal.table.TextTable


object Main extends App {
	val conn = new Connection
	val statement = """ {[a, b, c] (1, 2, 9), (3, 4, 8), (1, 5, 9), (3, 6, 0)} <a> (a, sum(b)) """

	conn.executeStatement( statement ) match {
		case TupleseqResult( tupleseq ) =>
			val t =
				new TextTable {
					tupleseq.header match {
						case None =>
						case Some( h ) =>
							headerSeq( h )
							line

							for (i <- 1 to h.length)
								if (tupleseq.types( i - 1 ).isInstanceOf[NumericalType])
									rightAlignment( i )
					}

					for (r <- tupleseq)
						rowSeq( r )
				}

			print( t )

			tupleseq.size match {
				case 0 => println( "empty tupleseq" )
				case 1 => println( "1 tuple" )
				case s => println( s"$s tuples" )
			}
		case RelationResult( rel ) =>
			val l = rel.collect
			val t =
				new TextTable {
					headerSeq( l.metadata.header map (_.column) )
					line

					for (i <- 1 to l.metadata.header.length)
						if (l.metadata.header( i - 1 ).typ.isInstanceOf[NumericalType])
							rightAlignment( i )

					for (r <- l)
						rowSeq( r )
				}

			print( t )

			l.size match {
				case 0 => println( "empty relation" )
				case 1 => println( "1 row" )
				case s => println( s"$s rows" )
			}
		case AssignResult( name, update, count ) =>
			println(
				(count, update) match {
					case (0, false) => s"empty variable relation '$name' was created"
					case (0, true) => s"variable relation '$name' was updated with empty relation"
					case (1, false) => s"variable relation '$name' was created with 1 row"
					case (1, true) => s"variable relation '$name' was updated with 1 row"
					case (_, false) => s"variable relation '$name' was created with $count rows"
					case (_, true) => s"variable relation '$name' was updated with $count rows"
				} )
		case InsertResult( _, count, created ) =>
			println(
				(count, created) match {
					case (0, Some(name)) => s"base relation '$name' was created"
					case (0, None) => "no rows were inserted"
					case (1, Some(name)) => s"base relation '$name' was created with 1 row"
					case (1, None) => "1 row was inserted"
					case (_, Some(name)) => s"base relation '$name' was created with $count rows"
					case (_, None) => s"$count rows were inserted"
				} )
		case DeleteResult( 0 ) => println( "no rows were deleted" )
		case DeleteResult( 1 ) => println( "1 row was deleted" )
		case DeleteResult( count ) => println( s"$count rows were deleted" )
	}

	/*
	{[a, b] (1, 2), (3, 4)}
	{[a, b]}
	{[a: integer, b]}
	{[a: integer, b: string]}
	{[a: float, b] (1, 2), (3, 4)}
	{[a, b] ('asdf', 123)}
	{[a, b] ('asdf', 123), ('kjhdfg', 5)}
	{[a, b] ('asdf', 123), (1, 2)}
	{[a, b] (1, A)}
	{[a, b] (1, A), (I, 'asdf')}
	{[a: integer, b: string] (1, 'asdf'), (3, 'zxcv')}

	[(1, 'asdf'), (3, 'zxcv')]

	insert r1 {[a, b] ('a', 2), ('b', 2), ('c', 1)}
	insert r2 {[c, d] (1, 'x'), (2, 'y'), (3, 'z')}

	r1 [b = c] r2
	{[a, b] ('a', 2), ('b', 2), ('c', 1)} [b = c] {[c, d] (1, 'x'), (2, 'y'), (3, 'z')} [d = 'x'] (a, b, d)

	a <- {[a, b] (1, 2), (3, 4)}

	{[a, b] (1, 2), (3, 4)} [a>1]
	{[a, b] (1, 2), (3, 4)} (a)
	{[a, b] (1, 2), (2, 4)} (float(avg(a)))
	{[a, b] (1, 2), (-3, 4)} (sum(a))
	{[a, b] (1, 2), (3, 4)} (count())

	{[a, b] (1, 2), (0, 5), (3, 4)} order by a
	*/
}