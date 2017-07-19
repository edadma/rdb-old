package xyz.hyperreal.rdb


object Main extends App {
	val conn = new Connection
	val statement = """ {[a, b] (1, 2), (3, 4)} [exists {[a, b] (1, 2)} [a = 1]] """

	REPLMain.printResult( conn.executeRQLStatement(statement) )

	/*
	Products [Products.CategoryID = Categories.CategoryID] Categories <CategoryName> (CategoryName, sum(Price))

	{[a, b, c] (1, 2, 9), (3, 4, 8), (1, 5, 9), (3, 6, 0)} [b > 2] <a> [a < 2] (a, sum(b))

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

	create r1 [a: string*, b: integer]
	insert r1 {[a, b] ('a', 2), ('b', 2), ('c', 1)}
	create r2 [c: integer*, d: string]
	insert r2 {[c, d] (1, 'x'), (2, 'y'), (3, 'z')}

	r1 [b = c] r2
	{[a, b] ('a', 2), ('b', 2), ('c', 1)} [b = c] {[c, d] (1, 'x'), (2, 'y'), (3, 'z')} [d = 'x'] (a, b, d)

	a <- {[a, b] (1, 2), (3, 4)}

	{[a, b] (1, 2), (3, 4)} [a>1]
	{[a, b] (1, 2), (3, 4)} (a)
	{[a, b] (1, 2), (2, 4)} (float(avg(a)))
	{[a, b] (1, 2), (-3, 4)} (sum(a))
	{[a, b] (1, 2), (3, 4)} (count())

	{[a, b, c] (1, 2, 9), (3, 4, 8), (1, 5, 9), (3, 6, 0)} <a> [a < 2] (a, sum(b))

	{[a, b, c] (1, 2, 7), (0, 5, 7), (3, 4, 6)} order by a
	*/
}