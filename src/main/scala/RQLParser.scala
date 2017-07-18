package xyz.hyperreal.rdb

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers

import xyz.hyperreal.lia.Math


object RQLParser {

	def parseStatement( statement: String ) = {
		val p = new RQLParser

		p.parseFromString( statement, p.statement )
	}

}

class RQLParser extends RegexParsers {
	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number: Parser[ValueExpression] = positioned( """\-?\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => FloatLit( n )
		case n => IntegerLit( n ) } )

	def string: Parser[ValueExpression] =
		positioned(
			(("'" ~> """[^'\n]*""".r <~ "'") |
			("\"" ~> """[^"\n]*""".r <~ "\"")) ^^ StringLit )

	def ident = pos ~ """[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ { case p ~ n => Ident( p, n ) }

	def statement: Parser[StatementAST] =
		assignStatement |
		createStatement |
		insertStatement |
		updateStatement |
		deleteStatement |
		tupleseq |
		relation

	def assignStatement =
		(ident <~ "<-") ~ relation ^^ { case n ~ r => AssignRelationStatement( n, r ) }

	def createStatement =
		("create" ~> ident) ~ columnsDef ^^ { case n ~ c => CreateBaseRelationStatement( n, c ) }

	def insertStatement =
		("insert" ~> ident) ~ relation ^^ { case n ~ r => InsertRelationStatement( n, r ) } |
		("insert" ~> ident) ~ tupleseq ^^ { case n ~ t => InsertTupleseqStatement( n, t ) } |
		("insert" ~> ident) ~ tuple ^^ { case n ~ t => InsertTupleseqStatement( n, TupleseqLit(List(t)) ) }

	def deleteStatement =
		("delete" ~> ident) ~ ("[" ~> logicalExpression <~ "]") ^^ { case n ~ c => DeleteStatement( n, c ) }

	def updateStatement =
		("update" ~> ident) ~ ("[" ~> logicalExpression <~ "]") ~ ("(" ~> rep1sep(ident ~ ("=" ~> valueExpression), ",") <~ ")") ^^ {
			case n ~ c ~ u => UpdateStatement( n, c, u map {case f ~ e => (f, e)} ) }

	def tupleseq =
		relation ~ ("order" ~ "by" ~> names) ~ opt("asc" | "desc") ^^ {
			case r ~ n ~ None => SortedTupleseqExpression( r, n, true )
			case r ~ n ~ Some( o ) => SortedTupleseqExpression( r, n, o == "asc" ) } |
		tupleseqLit

	def tupleseqLit =
		"[" ~> rep1sep(tuple, ",") <~ "]" ^^ TupleseqLit

	def names =
		ident ^^ (List( _ )) |
		"(" ~> rep1sep(ident, ",") <~ ")"

	def relation: Parser[RelationExpression] =
		projectionRelation

	def projectionRelation: Parser[RelationExpression] = positioned(
		selectionRelation ~ ("<" ~> rep1sep(nonLogicalValueExpression, ",") <~ ">") ~ opt("[" ~> logicalExpression <~ "]") ~ ("(" ~> expressions <~ ")") ^^ {
			case r ~ d ~ f ~ c => GroupingRelationExpression( r, d, f, null, c ) } |
		selectionRelation ~ ("(" ~> expressions <~ ")") ^^ { case r ~ c => ProjectionRelationExpression( r, c ) } |
		selectionRelation
		)

	def selectionRelation: Parser[RelationExpression] = positioned(
		innerJoinRelation ~ ("[" ~> logicalExpression <~ "]") ^^ { case r ~ c => SelectionRelationExpression( r, c ) } |
		innerJoinRelation
		)

	def innerJoinRelation: Parser[RelationExpression] = positioned(
		relationPrimary ~ ("[" ~> logicalExpression <~ "]") ~ relationPrimary ^^ { case l ~ c ~ r => InnerJoinRelationExpression( l, c, r ) } |
		relationPrimary
		)

	def relationPrimary: Parser[RelationExpression] = positioned(
		"(" ~> relation <~ ")" |
		("{" ~> columnsSpec) ~ (repsep(tuple, ",") <~ "}") ^^ { case c ~ d => ListRelationExpression( c, d ) } |
		ident ^^ RelationVariableExpression
		)

	def columnsSpec = "[" ~> rep1sep(columnSpec, ",") <~ "]"

	def columnSpec =
		ident ~ (":" ~> ident) ^^ {
			case n ~ t => ColumnSpec( n, t.pos, Some(t.name) ) } |
		ident ~ pos ^^ {
			case n ~ tp => ColumnSpec( n, tp, None ) }

	def columnsDef = "[" ~> rep1sep(columnDef, ",") <~ "]"

	def columnDef =
		ident ~ (":" ~> ident) ~ opt(pos <~ "*") ^^ {
			case n ~ t ~ None => ColumnDef( n, t.pos, Some(t.name), null, null, null )
			case n ~ t ~ Some(p) => ColumnDef( n, t.pos, Some(t.name), p, null, null ) } |
		ident ~ pos ~ opt(pos <~ "*") ^^ {
			case n ~ tp ~ None => ColumnDef( n, tp, None, null, null, null )
			case n ~ tp ~ Some(pkp) => ColumnDef( n, tp, None, pkp, null, null ) }

	def tuple = positioned( "(" ~> expressions <~ ")" ^^ TupleExpression )

	def expressions = rep1sep(valueExpression, ",")

	def alias( expr: Parser[ValueExpression] ): Parser[ValueExpression] =
		expr ~ ("=>" ~> ident) ^^ { case e ~ a => AliasValueExpression( e, a ) }

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
			case l ~ cs => ComparisonLogicalExpression( l, cs map { case c ~ v => (c, lookup(c), v)} ) } |
		"exists" ~> relation ^^ ExistsLogicalExpression |
		logicalPrimary

	def logicalPrimary = positioned(
		"true" ^^^ LiteralLogicalExpression( TRUE ) |
		"false" ^^^ LiteralLogicalExpression( FALSE ) |
		"maybe-a" ^^^ LiteralLogicalExpression( MAYBE_A ) |
		"maybe-i" ^^^ LiteralLogicalExpression( MAYBE_I )
		)

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parseAll( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}