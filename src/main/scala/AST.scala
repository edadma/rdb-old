package xyz.hyperreal.rdb

import scala.util.parsing.input.{Positional, Position}


trait AST

trait StatementAST extends AST
case class InsertRelationStatement( target: Ident, relation: RelationExpression ) extends StatementAST
case class InsertTuplesetStatement( target: Ident, tupleset: List[List[ValueExpression]] ) extends StatementAST

trait ValueExpression extends AST with Positional
case class NumberLit( n: String ) extends ValueExpression
case class IntegerLit( n: String ) extends ValueExpression
case class StringLit( s: String ) extends ValueExpression
case class MarkLit( m: Mark ) extends ValueExpression
case class ValueVariableExpression( name: Ident ) extends ValueExpression

trait RelationExpression extends StatementAST with Positional
case class RelationVariableExpression( name: Ident ) extends RelationExpression
case class ListRelationExpression( columns: List[ColumnSpec], data: List[List[ValueExpression]] ) extends RelationExpression
case class ProjectionRelationExpression( relation: RelationExpression, columns: List[Ident] ) extends RelationExpression
case class SelectionRelationExpression( relation: RelationExpression, condition: LogicalExpression ) extends RelationExpression

trait LogicalExpression extends Positional
case class LogicalLit( l: Logical ) extends LogicalExpression
case class ComparisonExpression( left: ValueExpression, comp: List[(String, ValueExpression)] ) extends LogicalExpression

case class Ident( pos: Position, name: String )

case class ColumnSpec( name: Ident, typepos: Position, typ: Option[String], pk: Boolean )