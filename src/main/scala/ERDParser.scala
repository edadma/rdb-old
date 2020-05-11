package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.{CharSequenceReader, Positional}
import util.parsing.combinator.RegexParsers

object ERDParser {

  def parseStatement(query: String) = {
    val p = new OQLParser

    p.parseFromString(query, p.query)
  }

}

class ERDParser extends RegexParsers {

  def pos = positioned(success(new Positional {})) ^^ {
    _.pos
  }

  def number: Parser[ExpressionERD] =
    positioned("""(?:\d+(?:\.\d+)?|\.\d+)(?:(?:e|E)(?:\+|-)?\d+)?""".r ^^ {
      case n if (n contains '.') || (n contains 'e') || (n contains 'E') =>
        FloatLiteralERD(n)
      case n => IntegerLiteralERD(n)
    })

  def string =
    positioned(
      (("'" ~> """[^'\n]*""".r <~ "'") |
        ("\"" ~> """[^"\n]*""".r <~ "\"")) ^^ StringLiteralERD)

  def ident =
    positioned("""[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ Ident)

  def variable = ident ^^ VariableExpressionERD

  def definition = rep1(block) ^^ DefinitionERD

  def block = typeBlock | entityBlock

  def typeBlock =
    "type" ~> ident ~ "=" ~ ident ~ ":" ~ condition ^^ {
      case n ~ _ ~ u ~ _ ~ c => TypeBlockERD(n, u, c)
    }

  def condition = boolCondition

  def boolCondition =
    orCondition ~ rep("and" ~> orCondition) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) { case (l, r) => AndExpressionERD(l, r) }
    }

  def orCondition =
    compCondition ~ rep("or" ~> compCondition) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) { case (l, r) => OrExpressionERD(l, r) }
    }

  def compCondition =
    positioned(notCondition ~ rep(("<" | "<=") ~ notCondition) ^^ {
      case first ~ Nil => first
      case first ~ rest =>
        ComparisonExpressionERD(first, rest map { case c ~ r => (c, r) })
    })

  def notCondition =
    positioned("not" ~> primaryCondition ^^ NotExpressionERD) |
      primaryCondition

  def primaryCondition: Parser[ExpressionERD] = variable | number

  def entityBlock = "entity" ~ ident ~ "{" ~ rep1(field) ~ "}"

  def field = ident ~ ":" ~ typeSpec

  def typeSpec = ident ^^ SimpleTypeERD

  def parseFromString[T](src: String, grammar: Parser[T]) =
    parseAll(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }

}

/*

type Language = text: _ is in ["English", "French", "Ongota"]

type Country = text: _ is in ["UK", "US", "JP"]

type PosInt = integer: _ > 0

entity movie {
 *mov_id: integer
  mov_title: text
  mov_year: integer
  mov_time: integer //PosInt
  mov_lang: Language
  mov_dt_rel: date
  mov_rel_country: text //Country
}

entity actor {
 *act_id: integer
  act_fname: text
  act_lname: text
  act_gender: text
}

entity movie_cast {
  actor: actor
  movie: movie
  role: text
}

entity director {
  dir_fname: text
  dir_lname: text
  movies: [movie] (movie_direction)
}

entity genres {
  gen_title: text
}



 */
