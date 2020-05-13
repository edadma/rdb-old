package xyz.hyperreal.rdb_sjs

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Positional}

object OQLParser {

  def parseQuery(query: String) = {
    val p = new OQLParser

    p.parseFromString(query, p.query)
  }

}

class OQLParser extends RegexParsers {

  override protected val whiteSpace: Regex = """(\s|;.*)+""".r

  def pos = positioned(success(new Positional {})) ^^ {
    _.pos
  }

  def number: Parser[ValueExpression] =
    positioned("""\-?\d+(\.\d*)?""".r ^^ {
      case n if n contains '.' => FloatLit(n)
      case n                   => IntegerLit(n)
    })

  def string: Parser[ValueExpression] =
    positioned(
      (("'" ~> """[^'\n]*""".r <~ "'") |
        ("\"" ~> """[^"\n]*""".r <~ "\"")) ^^ StringLit)

  def ident =
    positioned("""[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ Ident)

  def query =
    ident ~ opt(project) ~ opt(select) ~ opt(order) ~ opt(group) ^^ {
      case r ~ None ~ s ~ o ~ g    => OQLQuery(r, ProjectAllOQL, s, o, g)
      case r ~ Some(p) ~ s ~ o ~ g => OQLQuery(r, p, s, o, g)
    }

  def project: Parser[ProjectExpressionOQL] =
    "{" ~> rep1sep(projectExpression, ",") <~ "}" ^^ (ps =>
      ProjectFieldsOQL(ps)) |
      ("." | "^") ~ projectExpression ^^ {
        case "." ~ p => ProjectFieldsOQL(List(p))
        case _ ~ p   => ProjectLiftedOQL(p)
      }

  def attribute = ident ^^ AttributeOQL

  def projectExpression: Parser[ProjectExpressionOQL] =
    attributeProject | attribute

  def attributeProject = ident ~ project ^^ {
    case i ~ p => ProjectionExpressionOQL(i, p)
  }

  def variable = rep1sep(ident, ".") ^^ VariableExpressionOQL

  def expression = variable

  def select = "[" ~> expression <~ "]"

  def order = "<" ~> rep1sep(orderExpression, ",") <~ ">"

  def orderExpression = expression ~ opt("/" | "\\") ^^ {
    case e ~ (Some("/") | None) => (e, true)
    case e ~ _                  => (e, false)
  }

  def group = "(" ~> rep1sep(expression, ",") <~ ")"

  def parseFromString[T](src: String, grammar: Parser[T]) =
    parseAll(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }

}
