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

  def definition = rep1(block) ^^ DefinitionERD

  def block = typeBlock | entityBlock

  def typeBlock =
    "type" ~ "=" ~ ident ~ ":" ~ condition

  def condition = boolCond

  def boolCond =
    orCond ~ rep("and" ~ orCond)

  def orCond =
    compCond ~ rep("or" ~ compCond)

  def compCond =
    notCond ~ rep(("<" | "<=") ~ notCond)

  def notCond =
    "not" ~ primaryCond |
      primaryCond

  def primaryCond = ident | number

  def entityBlock = "entity" ~ ident ~ "{" ~ rep

//  def parseFromString[T](src: String, grammar: Parser[T]) =
//    parseAll(grammar, new CharSequenceReader(src)) match {
//      case Success(tree, _)       => tree
//      case NoSuccess(error, rest) => problem(rest.pos, error)
//    }

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
