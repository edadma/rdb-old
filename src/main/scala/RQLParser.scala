package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers


class RQLParser extends RegexParsers {
	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number = pos ~ """\d+(\.\d*)?""".r ^^ {
		case p ~ n if n contains '.' => NumberLit( p, n )
		case p ~ n => IntegerLit( p, n ) }

	def string = pos ~ ("'" ~> """[^'\n]+""".r <~ "'") ^^ { case p ~ s => StringLit( p, s ) }

	def ident = pos ~ """[a-zA-Z_]+""".r ^^ { case p ~ s => Ident( p, s ) }

	def relation =
		relationLiteral

	def relationLiteral =
		("{" ~> columns) ~ (rep(tuple) <~ "}") ^^ { case c ~ d => RelationLit( c, d ) }

	def columns = "[" ~> rep1sep(column, ",") <~ "]"

	def column = ident ~ opt(":" ~> ident) ^^ { case (n ~ t) => ColumnSpec( n, t ) }

	def tuple = "(" ~> rep1sep(valueExpression, ",") <~ ")"

	def valueExpression =
		valuePrimary

	def valuePrimary =
		number |
		string

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parse( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}