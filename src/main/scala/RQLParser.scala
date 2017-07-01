package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers

import xyz.hyperreal.lia.Math


class RQLParser extends RegexParsers {
	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number: Parser[ValueExpression] = positioned( """\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => FloatLit( n )
		case n => IntegerLit( n ) } )

	def string: Parser[ValueExpression] =
		positioned(
			(("'" ~> """[^'\n]+""".r <~ "'") |
			("\"" ~> """[^"\n]+""".r <~ "\"")) ^^ StringLit )

	def ident = pos ~ """[a-zA-Z_$][a-zA-Z0-9_#$]*""".r ^^ { case p ~ n => Ident( p, n ) }

	def statement: Parser[StatementAST] =
		insertStatement |
		relation

	def insertStatement =
		(ident <~ "<-") ~ columns ^^ { case n ~ c => InsertRelationStatement( n, ListRelationExpression(c, Nil) ) } |
		(ident <~ "<-") ~ relation ^^ { case n ~ r => InsertRelationStatement( n, r ) } |
		(ident <~ "<-") ~ tupleset ^^ { case n ~ t => InsertTuplesetStatement( n, t ) } |
		(ident <~ "<-") ~ tuple ^^ { case n ~ t => InsertTuplesetStatement( n, List(t) ) }

	def tupleset =
		"{" ~> rep1sep(tuple, ",") <~ "}"

	def relation: Parser[RelationExpression] = positioned(
		relationPrimary ~ ("[" ~> logicalExpression <~ "]") ^^ { case r ~ c => SelectionRelationExpression( r, c ) } |
		relationPrimary ~ ("[" ~> rep1sep(ident, ",") <~ "]") ^^ { case r ~ c => ProjectionRelationExpression( r, c ) } |
		relationPrimary
		)

	def relationPrimary = positioned(
		("{" ~> columns) ~ (repsep(tuple, ",") <~ "}") ^^ { case c ~ d => ListRelationExpression( c, d ) } |
		ident ^^ RelationVariableExpression
		)

	def columns = "[" ~> rep1sep(column, ",") <~ "]"

	def column =
		(	ident <~ ":") ~ ident ~ opt( "(" ~ "pk" ~ ")" ) ^^ {
				case (n ~ t ~ pk) => ColumnSpec( n, t.pos, Some(t.name), pk isDefined )} |
			ident ~ pos ~ opt( "fk" ) ^^ {
				case (n ~ p ~ pk) => ColumnSpec( n, p, None, pk isDefined ) }

	def tuple = "(" ~> rep1sep(valueExpression, ",") <~ ")"

	def valueExpression =
		valuePrimary

	def valuePrimary =
		number |
		string |
		positioned( "A" ^^^ MarkLit(A) ) |
		positioned( "I" ^^^ MarkLit(I) ) |
		positioned( ident ^^ ValueVariableExpression )

	private def lookup( s: String ) =
		Math.lookup(Symbol(
			s match {
				case "=" => "=="
				case "/=" => "!="
				case _ => s
			} ) )

	def logicalExpression =
		valueExpression ~ rep1(("<" | "<=" | "=" | "/=" | ">" | ">=") ~ valueExpression) ^^ {
			case l ~ cs => ComparisonExpression( l, cs map {case c ~ v => (c, lookup(c), v)} ) } |
		logicalPrimary

	def logicalPrimary = positioned(
		"true" ^^^ LogicalLit( TRUE ) |
		"false" ^^^ LogicalLit( FALSE ) |
		"maybe-a" ^^^ LogicalLit( MAYBE_A ) |
		"maybe-i" ^^^ LogicalLit( MAYBE_I )
		)

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parseAll( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}