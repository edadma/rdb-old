package xyz.hyperreal.rdb

import scala.util.parsing.input.Position


trait ValueExpression
case class NumberLit( p: Position, n: String ) extends ValueExpression
case class IntegerLit( p: Position, n: String ) extends ValueExpression
case class StringLit( p: Position, s: String ) extends ValueExpression
case class Ident( p: Position, s: String ) extends ValueExpression

trait RelationExpression
//case class RelationLit( columns: List[ColumnSpec], data: List[List[ValueExpression]] ) extends RelationExpression
case class RelationLit( columns: List[Ident], data: List[List[ValueExpression]] ) extends RelationExpression

//case class ColumnSpec( name: Ident, typ: Option[Ident] )