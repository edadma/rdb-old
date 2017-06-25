package xyz.hyperreal.rdb

import scala.util.parsing.input.CharSequenceReader
import util.parsing.combinator.RegexParsers


class RQLParser extends RegexParsers {
	def number = """\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => ('number, n)
		case n => ('integer, n) }

	def string = "'" ~> """[^'\n]+""".r <~ "'" ^^ { ('string, _) }

	def ident = """[a-zA-Z_]+""".r ^^ { ('ident, _) }

	def relation =
		relationLiteral

	def relationLiteral =
		("{" ~> nameList) ~ (rep(tuple) <~ "}") ^^ { case h ~ d => ('table, h, d) }

	def nameList = "[" ~> rep1sep(ident, ",") <~ "]"

	def tuple = "(" ~> rep1sep(valueExpression, ",") <~ ")" ^^ { ('tuple, _) }

	def valueExpression =
		primary

	def primary =
		number |
		string

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parse( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}