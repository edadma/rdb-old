package xyz.hyperreal.rdb_sjs

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Positional}

object RQLParser {

  def parseStatement(statement: String) = {
    val p = new RQLParser

    p.parseFromString(statement, p.statement)
  }

}

class RQLParser extends RegexParsers {
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

  def statement: Parser[StatementAST] =
    assignStatement |
      tableManipulationStatement |
      tupleseq |
      relation

  def assignStatement =
    (ident <~ "<-") ~ relation ^^ {
      case n ~ r => AssignRelationStatement(n, r)
    }

  def tableManipulationStatement =
    ("create" ~> ident) ~ columnsDef ^^ {
      case n ~ c => CreateBaseRelationStatement(n, c)
    } |
      "drop" ~> ident ^^ DropTableStatement |
      ("insert" ~> ident) ~ relation ^^ {
        case n ~ r => InsertRelationStatement(n, r)
      } |
      ("insert" ~> ident) ~ tupleseq ^^ {
        case n ~ t => InsertTupleseqStatement(n, t)
      } |
      ("insert" ~> ident) ~ tuple ^^ {
        case n ~ t => InsertTupleStatement(n, t)
      } |
      ("delete" ~> ident) ~ ("[" ~> logicalExpression <~ "]") ^^ {
        case n ~ c => DeleteStatement(n, c)
      } |
      ("update" ~> ident) ~ ("[" ~> logicalExpression <~ "]") ~ ("(" ~> rep1sep(ident ~ ("=" ~> valueExpression), ",") <~ ")") ^^ {
        case n ~ c ~ u => UpdateStatement(n, c, u map { case f ~ e => (f, e) })
      }

  def tupleseq =
//    relation ~ ("order" ~ "by" ~> orderby) ^^ {
//      case r ~ fs => SortedRelationExpression(r, fs)
//    } |
    tupleseqLit

  def orderby =
    ("order" ~ "by") ~> rep1sep(ident ~ opt(("ASC" | "asc") | ("DESC" | "desc")), ",") ^^ (l =>
      l.map {
        case i ~ None    => (i, true)
        case i ~ Some(a) => (i, a.toLowerCase == "asc")
      })

  def tupleseqLit =
    "[" ~> rep1sep(tuple, ",") <~ "]" ^^ TupleseqLit

  def names =
    ident ^^ (List(_)) |
      "(" ~> rep1sep(ident, ",") <~ ")"

  def relation: Parser[RelationExpression] =
    projectionRelation

  def projectionRelation: Parser[RelationExpression] = positioned(
    selectionRelation ~ ("<" ~> rep1sep(nonLogicalValueExpression, ",") <~ ">") ~ opt("[" ~> logicalExpression <~ "]") ~ ("(" ~> expressions <~ ")") ^^ {
      case r ~ d ~ f ~ c => GroupingRelationExpression(r, d, f, null, c)
    } |
      selectionRelation ~ ("(" ~> expressions <~ ")") ^^ {
        case r ~ c => ProjectionRelationExpression(r, c)
      } |
      selectionRelation
  )

  def selectionRelation: Parser[RelationExpression] = positioned(
    innerJoinRelation ~ ("[" ~> logicalExpression <~ "]") ^^ {
      case r ~ c => SelectionRelationExpression(r, c)
    } |
      innerJoinRelation
  )

  def innerJoinRelation: Parser[RelationExpression] = positioned(
    relationPrimary ~ ("[" ~> logicalExpression <~ "]") ~ relationPrimary ^^ {
      case l ~ c ~ r => InnerJoinRelationExpression(l, c, r)
    } |
      relationPrimary
  )

  def relationPrimary: Parser[RelationExpression] = positioned(
    "(" ~> relation <~ ")" |
      ("{" ~> columnsSpec) ~ (repsep(tuple, ",") <~ "}") ^^ {
        case c ~ d => ListRelationExpression(c, d)
      } |
      ident ^^ RelationVariableExpression
  )

  def columnsSpec = "[" ~> rep1sep(columnSpec, ",") <~ "]"

  def columnSpec =
    ident ~ (":" ~> ident) ^^ {
      case n ~ t => ColumnSpec(n, t.pos, Some(t.name))
    } |
      ident ~ pos ^^ {
        case n ~ tp => ColumnSpec(n, tp, None)
      }

  def columnsDef = "[" ~> rep1sep(columnDef, ",") <~ "]"

  def columnDef =
    ident ~ pos ~ (":" ~> columnType) ~ ("->" ~> ident) ~ ("(" ~> ident <~ ")") ~ opt("unmarkable") ^^ {
      case n ~ tp ~ t ~ fkr ~ fkc ~ u =>
        ColumnDef(n, tp, t, null, fkr, fkc, u isDefined, false)
    } |
      ident ~ pos ~ (":" ~> columnType) ~ opt(pos <~ "*") ~ opt("unmarkable") ~ opt("auto") ^^ {
        case n ~ tp ~ t ~ None ~ u ~ a =>
          ColumnDef(n, tp, t, null, null, null, u isDefined, a isDefined)
        case n ~ tp ~ t ~ Some(p) ~ u ~ a =>
          ColumnDef(n, tp, t, p, null, null, u isDefined, a isDefined)
      }

  def columnType =
    "smallint" ^^^ SmallintType |
      "integer" ^^^ IntegerType |
      "text" ^^^ TextType

  def tuple = positioned("(" ~> expressions <~ ")" ^^ TupleExpression)

  def expressions = rep1sep(valueExpression, ",")

  def alias(expr: Parser[ValueExpression]): Parser[ValueExpression] =
    expr ~ ("=>" ~> ident) ^^ { case e ~ a => AliasValueExpression(e, a) }

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
      "(" ~> valueExpression <~ ")" |
      positioned("A" ^^^ MarkLit(A)) |
      positioned("I" ^^^ MarkLit(I)) |
      positioned(ident ~ ("." ~> ident) ^^ {
        case t ~ c => ValueColumnExpression(t, c)
      }) |
      positioned(ident ^^ ValueVariableExpression)

  def comparison = "<=" | "<" | "=" | "!=" | ">=" | ">"

  def logicalExpression =
    orExpression

  def orExpression =
    andExpression ~ rep("or" ~> andExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => OrLogicalExpression(l, r)
        }
    }

  def andExpression =
    comparisonExpression ~ rep("and" ~> comparisonExpression) ^^ {
      case expr ~ list =>
        list.foldLeft(expr) {
          case (l, r) => AndLogicalExpression(l, r)
        }
    }

  def comparisonExpression =
    nonLogicalValueExpression ~ rep1(pos ~ comparison ~ nonLogicalValueExpression) ^^ {
      case l ~ cs =>
        ComparisonLogicalExpression(l, cs map {
          case p ~ c ~ v => (p, c, v)
        })
    } |
      ("EXISTS" | "exists") ~> relation ^^ ExistsLogicalExpression |
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
