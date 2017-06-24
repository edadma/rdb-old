package xyz.hyperreal.rdb

import util.parsing.combinator.RegexParsers


class RQLParser extends RegexParsers {
	def number = """\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => ('number, n)
		case n => ('integer, n) }

	def string = "'" ~> """[^'\n]+""".r <~ "'" ^^ { ('string, _) }

	def ident = """[a-zA-Z_]+""".r ^^ { ('ident, _) }

	def tableLiteral =
		("{" ~> nameList) ~ (rep(tuple) <~ "}") ^^ { case h ~ d => ('table, h, d) }

	def nameList = "[" ~> rep1sep(ident, ",") <~ "]"

	def tuple = "(" ~> rep1sep(valueExpression, ",") <~ ")" ^^ { ('tuple, _) }

	def valueExpression =
		primary

	def primary =
		number |
		string

	def apply( input: String ) =
		parseAll( tableLiteral, input ) match {
			case Success( result, _ ) => result
			case failure: NoSuccess => scala.sys.error( failure.msg )
		}

}