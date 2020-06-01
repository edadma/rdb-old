package xyz.hyperreal.rdb_sjs

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Positional}

object SQLParser {

  def parseStatement(statement: String) = {
    val p = new SQLParser

    p.parseFromString(statement, p.query)
  }

}

class SQLParser extends RegexParsers {

  override protected val whiteSpace: Regex = """(\s|--.*)+""".r

  def pos = positioned(success(new Positional {})) ^^ { _.pos }

  def integer: Parser[Int] =
    """\d+""".r ^^ (_.toInt)

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
    (("CREATE" | "create") ~ ("TABLE" | "table")) ~> ident ~ ("(" ~> rep1sep(columnDefinition, ",") <~ ")") ^^ {
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
    (("SELECT" | "select") ~> pos ~ (expressions | "*" ^^^ Nil) <~ ("FROM" | "from")) ~ fromRelation ~ opt(where) ~ opt(
      groupby) ~ opt(orderby) ~ opt(("LIMIT" | "limit") ~> integer) ~ opt(("OFFSET" | "offset") ~> integer) ^^ {
      case _ ~ Nil ~ r ~ None ~ None ~ None ~ None ~ None => r
      case _ ~ e ~ r ~ None ~ None ~ None ~ None ~ None   => ProjectionRelationExpression(r, e)
      case _ ~ Nil ~ r ~ Some(w) ~ None ~ None ~ None ~ None =>
        SelectionRelationExpression(r, w)
      case _ ~ e ~ r ~ Some(w) ~ None ~ None ~ None ~ None =>
        ProjectionRelationExpression(SelectionRelationExpression(r, w), e)
      case p ~ e ~ r ~ None ~ Some(d ~ h) ~ None ~ None ~ None =>
        GroupingRelationExpression(r, d, h, p, e)
      case p ~ e ~ r ~ Some(w) ~ Some(d ~ h) ~ None ~ None ~ None =>
        GroupingRelationExpression(SelectionRelationExpression(r, w), d, h, p, e)
      case _ ~ Nil ~ r ~ None ~ None ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(r, fs)
      case _ ~ Nil ~ r ~ None ~ None ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(SortedRelationExpression(r, fs), l, o)
      case _ ~ e ~ r ~ None ~ None ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(ProjectionRelationExpression(r, e), fs)
      case _ ~ e ~ r ~ None ~ None ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(SortedRelationExpression(ProjectionRelationExpression(r, e), fs), l, o)
      case _ ~ Nil ~ r ~ Some(w) ~ None ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(SelectionRelationExpression(r, w), fs)
      case _ ~ Nil ~ r ~ Some(w) ~ None ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(SortedRelationExpression(SelectionRelationExpression(r, w), fs), l, o)
      case _ ~ e ~ r ~ Some(w) ~ None ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(ProjectionRelationExpression(SelectionRelationExpression(r, w), e), fs)
      case _ ~ e ~ r ~ Some(w) ~ None ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(
          SortedRelationExpression(ProjectionRelationExpression(SelectionRelationExpression(r, w), e), fs),
          l,
          o)
      case p ~ e ~ r ~ None ~ Some(d ~ h) ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(GroupingRelationExpression(r, d, h, p, e), fs)
      case p ~ e ~ r ~ None ~ Some(d ~ h) ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(SortedRelationExpression(GroupingRelationExpression(r, d, h, p, e), fs), l, o)
      case p ~ e ~ r ~ Some(w) ~ Some(d ~ h) ~ Some(fs) ~ None ~ None =>
        SortedRelationExpression(GroupingRelationExpression(SelectionRelationExpression(r, w), d, h, p, e), fs)
      case p ~ e ~ r ~ Some(w) ~ Some(d ~ h) ~ Some(fs) ~ l ~ o =>
        LimitOffsetRelationExpression(
          SortedRelationExpression(GroupingRelationExpression(SelectionRelationExpression(r, w), d, h, p, e), fs),
          l,
          o)
    }

  def fromRelation =
    joinRelation |
      relation

  def joinRelation: Parser[RelationExpression] =
    primaryRelation ~ rep(
      (((("LEFT" | "left") <~ opt("OUTER" | "outer")) | opt("INNER" | "inner")) ~ (("JOIN" | "join") ~> primaryRelation)) ~ (("ON" | "on") ~> logicalExpression)) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, (None | Some(_)) ~ r ~ c)  => InnerJoinRelationExpression(l, c, r)
          case (l, ("LEFT" | "left") ~ r ~ c) => LeftJoinRelationExpression(l, c, r)
        }
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
    (("GROUP" | "group") ~ ("BY" | "by")) ~> expressions ~ opt(("HAVING" | "having") ~> logicalExpression)

  def orderby =
    ("ORDER" | "order") ~ ("BY" | "by") ~> rep1sep(valueExpression ~ opt(("ASC" | "asc") | ("DESC" | "desc")), ",") ^^ (
        l =>
          l.map {
            case i ~ None    => (i, 1)
            case i ~ Some(a) => (i, if (a.toLowerCase == "asc") 1 else -1)
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
    multiplicativeExpression ~ rep(pos ~ "+" ~ multiplicativeExpression | pos ~ "-" ~ multiplicativeExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (x, p ~ o ~ y) => BinaryValueExpression(x, p, o, y)
        }
    }

  def multiplicativeExpression: Parser[ValueExpression] =
    negativeExpression ~ rep(pos ~ "*" ~ negativeExpression | pos ~ "/" ~ negativeExpression) ^^ {
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
      caseExpression |
      "(" ~> valueExpression <~ ")" |
      positioned(("NULL" | "null") ^^^ MarkLit(A)) |
//      positioned("I" ^^^ MarkLit(I)) |
      columnPrimary

  def caseExpression =
    positioned(("CASE" | "case") ~ rep1(when) ~ opt(("ELSE" | "else") ~> valueExpression) ~ ("END" | "end") ^^ {
      case _ ~ ws ~ e ~ _ => CaseValueExpression(ws, e)
    })

  def when: Parser[(LogicalExpression, ValueExpression)] =
    ("WHEN" | "when") ~ logicalExpression ~ ("THEN" | "then") ~ valueExpression ^^ {
      case _ ~ c ~ _ ~ r => (c, r)
    }

  def columnPrimary =
    positioned(ident ~ ("." ~> ident) ^^ {
      case t ~ c => ValueColumnExpression(t, c)
    }) |
      positioned(ident ^^ ValueVariableExpression)

  def comparison = "<=" | "<" | "=" | "!=" | ">=" | ">"

  def logicalExpression: Parser[LogicalExpression] =
    orExpression

  def orExpression =
    andExpression ~ rep(("OR" | "or") ~> andExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => OrLogicalExpression(l, r)
        }
    }

  def andExpression =
    notExpression ~ rep(("AND" | "and") ~> notExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => AndLogicalExpression(l, r)
        }
    }

  def notExpression =
    ("NOT" | "not") ~> comparisonExpression ^^ (p => NotLogicalExpression(p)) |
      comparisonExpression

  def comparisonExpression: Parser[LogicalExpression] =
    nonLogicalValueExpression ~ rep1(pos ~ comparison ~ nonLogicalValueExpression) ^^ {
      case l ~ cs =>
        ComparisonLogicalExpression(l, cs map { case p ~ c ~ v => (p, c, v) })
    } |
      columnPrimary ~ pos ~ (("BETWEEN" | "between") ~> nonLogicalValueExpression <~ ("AND" | "and")) ~ nonLogicalValueExpression ^^ {
        case c ~ p ~ l ~ u =>
          ComparisonLogicalExpression(l, List((p, "<=", c), (p, "<=", u)))
      } |
      ("EXISTS" | "exists") ~> ("(" ~> query <~ ")") ^^ ExistsLogicalExpression |
      nonLogicalValueExpression ~ pos ~ opt("NOT" | "not") ~ ("LIKE" | "like" | "ILIKE" | "ilike") ~ nonLogicalValueExpression ^^ {
        case l ~ p ~ None ~ ("LIKE" | "like") ~ r =>
          LikeLogicalExpression(l, r, p, negated = false, casesensitive = true)
        case l ~ p ~ Some(_) ~ ("LIKE" | "like") ~ r =>
          LikeLogicalExpression(l, r, p, negated = true, casesensitive = true)
        case l ~ p ~ None ~ ("ILIKE" | "ilike") ~ r =>
          LikeLogicalExpression(l, r, p, negated = false, casesensitive = false)
        case l ~ p ~ Some(_) ~ ("ILIKE" | "ilike") ~ r =>
          LikeLogicalExpression(l, r, p, negated = true, casesensitive = false)
      } |
      nonLogicalValueExpression ~ (("IS" | "is") ~> opt("NOT" | "not") <~ ("NULL" | "null")) ^^ {
        case e ~ None    => IsNullLogicalExpression(e, negated = false)
        case e ~ Some(_) => IsNullLogicalExpression(e, negated = true)
      } |
      logicalPrimary

  def logicalPrimary: Parser[LogicalExpression] =
    positioned(
      ("TRUE" | "true") ^^^ LiteralLogicalExpression(TRUE) |
        ("FALSE" | "false") ^^^ LiteralLogicalExpression(FALSE)
    ) | "(" ~> logicalExpression <~ ")"

  def parseFromString[T](src: String, grammar: Parser[T]): T = {
    parseAll(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }
  }

}
// todo: use table names or aliases after "SELECT" (ex:https://www.w3schools.com/sql/sql_join_self.asp)
