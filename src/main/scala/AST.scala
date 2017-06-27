package xyz.hyperreal.rdb

import scala.util.parsing.input.{Positional, Position}


trait ValueExpression extends Positional
case class NumberLit( n: String ) extends ValueExpression
case class IntegerLit( n: String ) extends ValueExpression
case class StringLit( s: String ) extends ValueExpression
case class MarkLit( m: Mark ) extends ValueExpression

case class Ident( p: Position, s: String )

trait RelationExpression
case class RelationLit( columns: List[ColumnSpec], data: List[List[ValueExpression]] ) extends RelationExpression

case class ColumnSpec( name: Ident, typ: Option[Ident] )