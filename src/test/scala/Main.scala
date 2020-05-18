package xyz.hyperreal.rdb_sjs

import scala.scalajs.js
import js.Dynamic.{global => g}

object Main extends App {
  private val fs = g.require("fs")

  private def readFile(name: String) = {
    fs.readFileSync(name).toString
  }

  val conn = new Connection {
    load(readFile("samples/student.tab"), doubleSpaces = true)
//    load(readFile("samples/star_trek.tab"), doubleSpaces = true)
  }

  val oql = new OQL(readFile("samples/student.erd"))
  val res = oql.query("student", conn)
//  val oql = new OQL(readFile("samples/star_trek.erd"))
//  val res = oql.query("character [species.lifespan < 150] <home.name>", conn)
  println(OQL.pretty(res))
//  val conn = new Connection { load(readFile("samples/northwind.tab"), true) }
//  val statement =
//    """
//			|SELECT CompanyName, ContactName
//			|  FROM Suppliers
//			|  WHERE EXISTS (SELECT * FROM Products WHERE SupplierID = Suppliers.SupplierID AND UnitPrice < 10)
//		""".stripMargin

//		"""
//			|SELECT SupplierName
//			|  FROM Suppliers
//			|  WHERE EXISTS (SELECT ProductName FROM Products WHERE SupplierID = Suppliers.SupplierID AND Price < 5)
//		""".stripMargin

//		"""
//			|select * from Products where Price between 15 and 20 order by Price desc
//		""".stripMargin

//	"""
//		|{[a, b, c] (1, 2, 9), (3, 4, 8), (1, 5, 9), (3, 6, 0)} [2 < b and b < 6]
//	""".stripMargin

//    println( SQLParser.parseStatement(statement) )
//  REPLMain.printResult(conn.executeSQLStatement(statement))

  /*
	Products [Products.CategoryID = Categories.CategoryID] Categories <CategoryName> (CategoryName, sum(Price))

	{[a, b, c] (1, 2, 9), (3, 4, 8), (1, 5, 9), (3, 6, 0)} [b > 2] <a> [a < 2] (a, sum(b))

	{[a, b] (1, 2), (3, 4)}
	{[a, b]}
	{[a: integer, b]}
	{[a: integer, b: text]}
	{[a: float, b] (1, 2), (3, 4)}
	{[a, b] ('asdf', 123)}
	{[a, b] ('asdf', 123), ('kjhdfg', 5)}
	{[a, b] ('asdf', 123), (1, 2)}
	{[a, b] (1, A)}
	{[a, b] (1, A), (I, 'asdf')}
	{[a: integer, b: text] (1, 'asdf'), (3, 'zxcv')}

	[(1, 'asdf'), (3, 'zxcv')]

	create r1 [a: text*, b: integer]
	insert r1 {[a, b] ('a', 2), ('b', 2), ('c', 1)}
	create r2 [c: integer* auto, d: text]
	insert r2 {[c, d] (1, 'x'), (2, 'y'), (3, 'z')}
	insert r2 {[d] ('asdf')}

	create r1 [c: integer* auto, d: text]
	create r2 [a: integer* auto, b: integer -> r1(c)]
	insert r1 {[d] ('asdf')}
	insert r2 {[b] (1)}

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
