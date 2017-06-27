package xyz.hyperreal.rdb

import scala.util.parsing.input.{Positional, Position}


trait ValueExpression extends Positional
case class NumberLit( n: String ) extends ValueExpression
case class IntegerLit( n: String ) extends ValueExpression
case class StringLit( s: String ) extends ValueExpression
case class MarkLit( m: Mark ) extends ValueExpression

trait RelationExpression
case class RelationVariable( name: String ) extends RelationExpression
case class RelationLit( columns: List[ColumnSpec], data: List[List[ValueExpression]] ) extends RelationExpression

case class ColumnSpec( namepos: Position, name: String, typepos: Position, typ: Option[String] )