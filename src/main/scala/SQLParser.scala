package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers

import xyz.hyperreal.lia.Math


object SQLParser {

	def parseStatement( statement: String ) = {
		val p = new SQLParser

		p.parseFromString( statement, p.query )
	}

}

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

	def query =
		("select" ~> pos ~ (expressions|"*" ^^^ Nil) <~ "from") ~ relation ~ opt(where) ~ opt(groupby) ^^ {
			case _ ~ Nil ~ r ~ None ~ None => r
			case _ ~ e ~ r ~ None ~ None => ProjectionRelationExpression( r, e )
			case _ ~ Nil ~ r ~ Some( w ) ~ None => SelectionRelationExpression( r, w )
			case _ ~ e ~ r ~ Some( w ) ~ None => ProjectionRelationExpression( SelectionRelationExpression(r, w), e )
			case p ~ e ~ r ~ None ~ Some( g ) => GroupingRelationExpression( r, g, p, e )
			case p ~ e ~ r ~ Some( w ) ~ Some( g ) => GroupingRelationExpression( SelectionRelationExpression(r, w), g, p, e )
		}

	def relation = ident ^^ RelationVariableExpression

	def where = "where" ~> logicalExpression

	def groupby = "group" ~ "by" ~> expressions

	def expressions = rep1sep(valueExpression, ",")

	def alias( expr: Parser[ValueExpression] ): Parser[ValueExpression] =
		expr ~ ("as" ~> ident) ^^ { case e ~ a => AliasValueExpression( e, a ) }

	def valueExpression: Parser[ValueExpression] =
		alias( lowPrecvalueExpression ) |
			lowPrecvalueExpression

	def lowPrecvalueExpression: Parser[ValueExpression] =
		logicalExpression ^^ LogicalValueExpression |
			additiveExpression

	def nonLogicalValueExpression: Parser[ValueExpression] =
		alias( additiveExpression ) |
			additiveExpression

	def additiveExpression: Parser[ValueExpression] =
		multiplicativeExpression ~ rep(pos ~ "+" ~ multiplicativeExpression | pos ~ "-" ~ multiplicativeExpression) ^^ {
			case expr ~ list => list.foldLeft( expr ) {
				case (x, p ~ o ~ y) => BinaryValueExpression( x, p, o, lookup(o), y )
			}
		}

	def multiplicativeExpression: Parser[ValueExpression] = negativeExpression ~ rep(pos ~ "*" ~ negativeExpression | pos ~ "/" ~ negativeExpression) ^^ {
		case expr ~ list => list.foldLeft( expr ) {
			case (x, p ~ o ~ y) => BinaryValueExpression( x, p, o, lookup(o), y )
		}
	}

	def negativeExpression: Parser[ValueExpression] =
		(pos <~ "-") ~ exponentialExpression ^^ { case p ~ e => UnaryValueExpression( p, "-", lookup("-"), e ) } |
			exponentialExpression

	def exponentialExpression: Parser[ValueExpression] =
		applicativeExpression ~ pos ~ ("^" ~> applicativeExpression) ^^ { case l ~ p ~ r => BinaryValueExpression( l, p, "^", lookup("^"), r ) } |
			applicativeExpression

	def applicativeExpression: Parser[ValueExpression] =
		positioned( valuePrimary ~ ("(" ~> repsep(valueExpression, ",") <~ ")") ^^ {
			case f ~ args => ApplicativeValueExpression( f, args ) } ) |
			valuePrimary

	def valuePrimary: Parser[ValueExpression] =
		number |
			string |
			"(" ~> valueExpression <~ ")" |
			positioned( "A" ^^^ MarkLit(A) ) |
			positioned( "I" ^^^ MarkLit(I) ) |
			positioned( ident ~ ("." ~> ident) ^^ { case t ~ c => ValueColumnExpression( t, c ) } ) |
			positioned( ident ^^ ValueVariableExpression )

	private def lookup( s: String ) =
		Math.lookup(Symbol(
			s match {
				case "=" => "=="
				case "/=" => "!="
				case _ => s
			} ) )

	def comparison = "<" | "<=" | "=" | "/=" | ">" | ">="

	def logicalExpression =
		nonLogicalValueExpression ~ rep1(comparison ~ nonLogicalValueExpression) ^^ {
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