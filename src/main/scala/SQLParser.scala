package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers

import xyz.hyperreal.lia.Math


class SQLParser extends RegexParsers {
	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number: Parser[ValueExpression] = positioned( """\-?\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => FloatLit( n )
		case n => IntegerLit( n ) } )

	def string: Parser[ValueExpression] =
		positioned(
			(("'" ~> """[^'\n]*""".r <~ "'") |
				("\"" ~> """[^"\n]*""".r <~ "\"")) ^^ StringLit )

	def ident = pos ~ """[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ { case p ~ n => Ident( p, n ) }



	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parseAll( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}