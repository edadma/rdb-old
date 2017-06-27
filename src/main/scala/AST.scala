package xyz.hyperreal.rdb

import scala.util.parsing.input.{Positional, Position}


trait ValueExpression extends Positional
case class NumberLit( n: String ) extends ValueExpression
case class IntegerLit( n: String ) extends ValueExpression
case class StringLit( s: String ) extends ValueExpression
case class MarkLit( m: Mark ) extends ValueExpression

trait RelationExpression
case class VariableRelationExpression( name: Name ) extends RelationExpression
case class LiteralRelationExpression( columns: List[ColumnSpec], data: List[List[ValueExpression]] ) extends RelationExpression
case class ProjectionRelationExpression( operand: RelationExpression, columns: List[Name] ) extends RelationExpression

case class Name( name: String ) extends Positional

case class ColumnSpec( namepos: Position, name: String, typepos: Position, typ: Option[String] )