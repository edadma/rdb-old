package xyz.hyperreal.rdb


object Main extends App {
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

	r1 <- {[a, b] ('a', 2), ('b', 2), ('c', 1)}
	r2 <- {[c, d] (1, 'x'), (2, 'y'), (3, 'z')}

	r1 [b = c] r2
	{[a, b] ('a', 2), ('b', 2), ('c', 1)} [b = c] {[c, d] (1, 'x'), (2, 'y'), (3, 'z')} [a, b, d]

	a <- {[a, b] (1, 2), (3, 4)}

	{[a, b] (1, 2), (3, 4)} (a)
	{[a, b] (1, 2), (3, 4)} (sum(a))
	{[a, b] (1, 2), (3, 4)} (count())
	*/
}