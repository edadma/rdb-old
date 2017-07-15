package xyz.hyperreal.rdb

import scala.util.parsing.input.{Positional, Position}

import xyz.hyperreal.lia.FunctionMap


trait AST

trait StatementAST extends AST
case class AssignRelationStatement( variable: Ident, relation: RelationExpression ) extends StatementAST
case class InsertRelationStatement( base: Ident, relation: RelationExpression ) extends StatementAST
case class InsertTupleseqStatement( base: Ident, tupleseq: TupleseqExpression ) extends StatementAST
case class DeleteStatement( base: Ident, condition: LogicalExpression ) extends StatementAST
case class UpdateStatement( base: Ident, condition: LogicalExpression, updates: List[(Ident, ValueExpression)] ) extends StatementAST

trait ValueExpression extends AST with Positional
case class FloatLit( n: String ) extends ValueExpression
case class IntegerLit( n: String ) extends ValueExpression
case class StringLit( s: String ) extends ValueExpression
case class MarkLit( m: Mark ) extends ValueExpression
case class ValueVariableExpression( name: Ident ) extends ValueExpression
case class ValueColumnExpression( table: Ident, column: Ident ) extends ValueExpression
case class TupleExpression( t: List[ValueExpression] ) extends ValueExpression
case class BinaryValueExpression( left: ValueExpression, oppos: Position, operation: String, func: FunctionMap, right: ValueExpression ) extends ValueExpression
case class ApplicativeValueExpression( func: ValueExpression, args: List[ValueExpression] ) extends ValueExpression
case class UnaryValueExpression( oppos: Position, operation: String, func: FunctionMap, expr: ValueExpression ) extends ValueExpression
case class LogicalValueExpression( logical: LogicalExpression ) extends ValueExpression
case class AliasValueExpression( expr: ValueExpression, alias: Ident ) extends ValueExpression

trait RelationExpression extends StatementAST with Positional
case class RelationVariableExpression( name: Ident ) extends RelationExpression
case class ListRelationExpression( columns: List[ColumnSpec], data: List[TupleExpression] ) extends RelationExpression
case class ProjectionRelationExpression( relation: RelationExpression, columns: List[ValueExpression] ) extends RelationExpression
case class SelectionRelationExpression( relation: RelationExpression, condition: LogicalExpression ) extends RelationExpression
case class InnerJoinRelationExpression( left: RelationExpression, condition: LogicalExpression, right: RelationExpression ) extends RelationExpression
case class GroupingRelationExpression( relation: RelationExpression, discriminator: List[ValueExpression], filter: Option[LogicalExpression], cpos: Position, columns: List[ValueExpression] ) extends RelationExpression

trait TupleseqExpression extends StatementAST with Positional
case class TupleseqLit( data: List[TupleExpression] ) extends TupleseqExpression
case class SortedTupleseqExpression( relation: RelationExpression, names: List[Ident], ascending: Boolean ) extends TupleseqExpression

trait LogicalExpression extends Positional
case class LogicalLit( l: Logical ) extends LogicalExpression
case class ComparisonExpression( left: ValueExpression, comp: List[(String, FunctionMap, ValueExpression)] ) extends LogicalExpression
case class LogicalAnd( left: LogicalExpression, right: LogicalExpression )
case class LogicalOr( left: LogicalExpression, right: LogicalExpression )

case class Ident( pos: Position, name: String )

case class ColumnSpec( name: Ident, typepos: Position, typ: Option[String], pkpos: Position, fkr: Ident, fkc: Ident/*, auto: Boolean*/ )