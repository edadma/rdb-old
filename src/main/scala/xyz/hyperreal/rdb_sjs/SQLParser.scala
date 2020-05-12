package xyz.hyperreal.rdb_sjs

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Positional}

object SQLParser {

  def parseStatement(statement: String) = {
    val p = new SQLParser

    p.parseFromString(statement, p.query)
  }

}

class SQLParser extends RegexParsers {
  def pos = positioned(success(new Positional {})) ^^ { _.pos }

  def number: Parser[ValueExpression] =
    positioned("""\-?\d+(\.\d*)?""".r ^^ {
      case n if n contains '.' => FloatLit(n)
      case n                   => IntegerLit(n)
    })

  def string: Parser[ValueExpression] =
    positioned(
      (("'" ~> """[^'\n]*""".r <~ "'") |
        ("\"" ~> """[^"\n]*""".r <~ "\"")) ^^ StringLit)

  def ident = positioned("""[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ Ident)

  def createTable =
    (("CREATE" | "create") ~ ("TABLE" | "table")) ~> ident ~ ("(" ~> rep1sep(
      columnDefinition,
      ",") <~ ")") ^^ {
      case name ~ types => CreateBaseRelationStatement(name, types)
    }

  def columnDefinition =
    ident ~ pos ~ columnType ~ opt("not" ~ "null") ~ opt("auto") ^^ {
      case name ~ pos ~ typ ~ u ~ a =>
        ColumnDef(name, pos, typ, null, null, null, u isDefined, a isDefined)
    }

  def columnType =
    "smallint" ^^^ SmallintType |
      "integer" ^^^ IntegerType |
      "text" ^^^ TextType |
      "date" ^^^ DateType |
      "timestamp" ^^^ InstantType

  def ascending(o: Option[String]) = o.isEmpty || o.get.toLowerCase == "asc"

  def query: Parser[TupleCollectionExpression] =
    (("SELECT" | "select") ~> pos ~ (expressions | "*" ^^^ Nil) <~ ("FROM" | "from")) ~ fromRelation ~ opt(
      where) ~ opt(groupby) ~ opt(orderby) ^^ {
      case _ ~ Nil ~ r ~ None ~ None ~ None => r
      case _ ~ e ~ r ~ None ~ None ~ None   => ProjectionRelationExpression(r, e)
      case _ ~ Nil ~ r ~ Some(w) ~ None ~ None =>
        SelectionRelationExpression(r, w)
      case _ ~ e ~ r ~ Some(w) ~ None ~ None =>
        ProjectionRelationExpression(SelectionRelationExpression(r, w), e)
      case p ~ e ~ r ~ None ~ Some(d ~ h) ~ None =>
        GroupingRelationExpression(r, d, h, p, e)
      case p ~ e ~ r ~ Some(w) ~ Some(d ~ h) ~ None =>
        GroupingRelationExpression(SelectionRelationExpression(r, w),
                                   d,
                                   h,
                                   p,
                                   e)
      case _ ~ Nil ~ r ~ None ~ None ~ Some(fs) =>
        SortedTupleseqExpression(r, fs)
      case _ ~ e ~ r ~ None ~ None ~ Some(fs) =>
        SortedTupleseqExpression(ProjectionRelationExpression(r, e), fs)
      case _ ~ Nil ~ r ~ Some(w) ~ None ~ Some(fs) =>
        SortedTupleseqExpression(SelectionRelationExpression(r, w), fs)
      case _ ~ e ~ r ~ Some(w) ~ None ~ Some(fs) =>
        SortedTupleseqExpression(
          ProjectionRelationExpression(SelectionRelationExpression(r, w), e),
          fs)
      case p ~ e ~ r ~ None ~ Some(d ~ h) ~ Some(fs) =>
        SortedTupleseqExpression(GroupingRelationExpression(r, d, h, p, e), fs)
      case p ~ e ~ r ~ Some(w) ~ Some(d ~ h) ~ Some(fs) =>
        SortedTupleseqExpression(
          GroupingRelationExpression(SelectionRelationExpression(r, w),
                                     d,
                                     h,
                                     p,
                                     e),
          fs)
    }

  def fromRelation =
    innerJoinRelation |
      relation

  def innerJoinRelation: Parser[RelationExpression] =
    (primaryRelation | "(" ~> innerJoinRelation <~ ")") ~ (opt(
      "INNER" | "inner") ~> ("JOIN" | "join") ~> primaryRelation) ~ (("ON" | "on") ~> logicalExpression) ^^ {
      case l ~ r ~ c => InnerJoinRelationExpression(l, c, r)
    }

  def relation: Parser[RelationExpression] = primaryRelation

  def primaryRelation =
    ident ~ ("AS" | "as") ~ ident ^^ {
      case t ~ _ ~ a =>
        AliasVariableExpression(RelationVariableExpression(t), a)
    } |
      ident ^^ RelationVariableExpression |
      "(" ~> relation <~ ")"

  def where = ("WHERE" | "where") ~> logicalExpression

  def groupby =
    (("GROUP" | "group") ~ ("BY" | "by")) ~> expressions ~ opt(
      ("HAVING" | "having") ~> logicalExpression)

  def orderby =
    ("ORDER" | "order") ~ ("BY" | "by") ~> rep1sep(
      ident ~ opt(("ASC" | "asc") | ("DESC" | "desc")),
      ",") ^^ (l =>
      l.map {
        case i ~ None    => (i, true)
        case i ~ Some(a) => (i, a.toLowerCase == "asc")
      })

  def expressions = rep1sep(valueExpression, ",")

  def alias(expr: Parser[ValueExpression]): Parser[ValueExpression] =
    expr ~ (("AS" | "as") ~> ident) ^^ {
      case e ~ a => AliasValueExpression(e, a)
    }

  def valueExpression: Parser[ValueExpression] =
    alias(lowPrecvalueExpression) |
      lowPrecvalueExpression

  def lowPrecvalueExpression: Parser[ValueExpression] =
    logicalExpression ^^ LogicalValueExpression |
      additiveExpression

  def nonLogicalValueExpression: Parser[ValueExpression] =
    alias(additiveExpression) |
      additiveExpression

  def additiveExpression: Parser[ValueExpression] =
    multiplicativeExpression ~ rep(
      pos ~ "+" ~ multiplicativeExpression | pos ~ "-" ~ multiplicativeExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (x, p ~ o ~ y) => BinaryValueExpression(x, p, o, y)
        }
    }

  def multiplicativeExpression: Parser[ValueExpression] =
    negativeExpression ~ rep(
      pos ~ "*" ~ negativeExpression | pos ~ "/" ~ negativeExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (x, p ~ o ~ y) => BinaryValueExpression(x, p, o, y)
        }
    }

  def negativeExpression: Parser[ValueExpression] =
    (pos <~ "-") ~ exponentialExpression ^^ {
      case p ~ e => UnaryValueExpression(p, "-", e)
    } |
      exponentialExpression

  def exponentialExpression: Parser[ValueExpression] =
    applicativeExpression ~ pos ~ ("^" ~> applicativeExpression) ^^ {
      case l ~ p ~ r => BinaryValueExpression(l, p, "^", r)
    } |
      applicativeExpression

  def applicativeExpression: Parser[ValueExpression] =
    positioned(valuePrimary ~ ("(" ~> repsep(valueExpression, ",") <~ ")") ^^ {
      case f ~ args => ApplicativeValueExpression(f, args)
    }) |
      valuePrimary

  def valuePrimary: Parser[ValueExpression] =
    number |
      string |
      "(" ~> valueExpression <~ ")" |
      positioned(("NULL" | "null") ^^^ MarkLit(A)) |
//      positioned("I" ^^^ MarkLit(I)) |
      columnPrimary

  def columnPrimary =
    positioned(ident ~ ("." ~> ident) ^^ {
      case t ~ c => ValueColumnExpression(t, c)
    }) |
      positioned(ident ^^ ValueVariableExpression)

  def comparison = "<" | "<=" | "=" | "!=" | ">" | ">="

  def logicalExpression =
    orExpression

  def orExpression =
    andExpression ~ rep(("OR" | "or") ~> andExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => OrLogicalExpression(l, r)
        }
    }

  def andExpression =
    comparisonExpression ~ rep(("AND" | "and") ~> comparisonExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => AndLogicalExpression(l, r)
        }
    }

  def comparisonExpression =
    nonLogicalValueExpression ~ rep1(
      pos ~ comparison ~ nonLogicalValueExpression) ^^ {
      case l ~ cs =>
        ComparisonLogicalExpression(l, cs map { case p ~ c ~ v => (p, c, v) })
    } |
      columnPrimary ~ pos ~ (("BETWEEN" | "between") ~> nonLogicalValueExpression <~ ("AND" | "and")) ~ nonLogicalValueExpression ^^ {
        case c ~ p ~ l ~ u =>
          ComparisonLogicalExpression(l, List((p, "<=", c), (p, "<=", u)))
      } |
      ("EXISTS" | "exists") ~> ("(" ~> query <~ ")") ^^ ExistsLogicalExpression |
      logicalPrimary

  def logicalPrimary = positioned(
    "true" ^^^ LiteralLogicalExpression(TRUE) |
      "false" ^^^ LiteralLogicalExpression(FALSE) |
      "maybe-a" ^^^ LiteralLogicalExpression(MAYBE_A) |
      "maybe-i" ^^^ LiteralLogicalExpression(MAYBE_I)
  )

  def parseFromString[T](src: String, grammar: Parser[T]) = {
    parseAll(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }
  }

}
//todo:use table names or aliases after "SELECT" (ex:https://www.w3schools.com/sql/sql_join_self.asp)
