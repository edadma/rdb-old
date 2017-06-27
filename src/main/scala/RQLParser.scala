package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers


class RQLParser extends RegexParsers {
	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number: Parser[ValueExpression] = positioned( """\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => NumberLit( n )
		case n => IntegerLit( n ) } )

	def string: Parser[ValueExpression] =
		positioned(
			(("'" ~> """[^'\n]+""".r <~ "'") |
			("\"" ~> """[^"\n]+""".r <~ "\"")) ^^ StringLit )

	def ident = pos ~ """[a-zA-Z_#$]+""".r ^^ { case p ~ n => Ident( p, n ) }

	def relation: Parser[RelationExpression] =
		relationVariable |
		relationLiteral |
		relationProjection

	def relationVariable =
		ident ^^ VariableRelationExpression

	def relationProjection =
		relation ~ ("[" ~> rep1sep(ident, ",") <~ "]") ^^ { case r ~ n => ProjectionRelationExpression( r, n ) }

	def relationLiteral =
		("{" ~> columns) ~ (rep(tuple) <~ "}") ^^ { case c ~ d => LiteralRelationExpression( c, d ) }

	def columns = "[" ~> rep1sep(column, ",") <~ "]"

	def column =
		(ident <~ ":") ~ ident ^^ {
				case (n ~ t) => ColumnSpec( n, t.pos, Some(t.name) )} |
			ident ~ pos ^^ {
				case (n ~ p) => ColumnSpec( n, p, None ) }

	def tuple = "(" ~> rep1sep(valueExpression, ",") <~ ")"

	def valueExpression =
		valuePrimary

	def valuePrimary =
		number |
		string |
		"A" ^^^ MarkLit( A ) |
		"I" ^^^ MarkLit( I )

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parse( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}