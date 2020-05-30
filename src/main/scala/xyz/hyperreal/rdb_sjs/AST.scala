package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.{Position, Positional}

abstract class AST

abstract class StatementAST extends AST
case class AssignRelationStatement(variable: Ident, relation: RelationExpression) extends StatementAST
case class CreateBaseRelationStatement(base: Ident, columns: List[ColumnDef]) extends StatementAST
case class DropTableStatement(base: Ident) extends StatementAST
case class InsertRelationStatement(base: Ident, relation: RelationExpression) extends StatementAST
case class InsertTupleseqStatement(base: Ident, tupleseq: TupleseqExpression) extends StatementAST
case class InsertTupleStatement(base: Ident, tupl: TupleExpression) extends StatementAST
case class DeleteStatement(base: Ident, condition: LogicalExpression) extends StatementAST
case class UpdateStatement(base: Ident, condition: LogicalExpression, updates: List[(Ident, ValueExpression)])
    extends StatementAST

abstract class ValueExpression extends AST with Positional
case class FloatLit(n: String) extends ValueExpression
case class IntegerLit(n: String) extends ValueExpression
case class StringLit(s: String) extends ValueExpression
case class MarkLit(m: Mark) extends ValueExpression
case class ValueVariableExpression(name: Ident) extends ValueExpression
case class ValueColumnExpression(table: Ident, column: Ident) extends ValueExpression
case class TupleExpression(t: List[ValueExpression]) extends ValueExpression
case class BinaryValueExpression(left: ValueExpression, oppos: Position, operation: String, right: ValueExpression)
    extends ValueExpression
case class ApplicativeValueExpression(func: ValueExpression, args: List[ValueExpression]) extends ValueExpression
case class UnaryValueExpression(oppos: Position, operation: String, expr: ValueExpression) extends ValueExpression
case class LogicalValueExpression(logical: LogicalExpression) extends ValueExpression
case class AliasValueExpression(expr: ValueExpression, alias: Ident) extends ValueExpression
case class CaseValueExpression(whens: List[(LogicalExpression, ValueExpression)], els: Option[ValueExpression])
    extends ValueExpression

abstract class TupleCollectionExpression extends StatementAST
abstract class RelationExpression extends TupleCollectionExpression with Positional
case class RelationVariableExpression(name: Ident) extends RelationExpression
case class AliasVariableExpression(rel: RelationExpression, alias: Ident) extends RelationExpression
case class ListRelationExpression(columns: List[ColumnSpec], data: List[TupleExpression]) extends RelationExpression
case class ProjectionRelationExpression(relation: RelationExpression, columns: List[ValueExpression])
    extends RelationExpression
case class SelectionRelationExpression(relation: RelationExpression, condition: LogicalExpression)
    extends RelationExpression
case class LimitOffsetRelationExpression(relation: RelationExpression, limit: Option[Int], offset: Option[Int])
    extends RelationExpression
case class InnerJoinRelationExpression(left: RelationExpression,
                                       condition: LogicalExpression,
                                       right: RelationExpression)
    extends RelationExpression
case class LeftJoinRelationExpression(left: RelationExpression, condition: LogicalExpression, right: RelationExpression)
    extends RelationExpression
case class GroupingRelationExpression(relation: RelationExpression,
                                      discriminator: List[ValueExpression],
                                      filter: Option[LogicalExpression],
                                      cpos: Position,
                                      columns: List[ValueExpression])
    extends RelationExpression
case class SortedRelationExpression(relation: RelationExpression, exprs: List[(ValueExpression, Int)])
    extends RelationExpression

abstract class TupleseqExpression extends TupleCollectionExpression with Positional
case class TupleseqLit(data: List[TupleExpression]) extends TupleseqExpression

abstract class LogicalExpression extends Positional
case class LiteralLogicalExpression(l: Logical) extends LogicalExpression
case class ComparisonLogicalExpression(left: ValueExpression, comp: List[(Position, String, ValueExpression)])
    extends LogicalExpression
case class LikeLogicalExpression(left: ValueExpression, lpos: Position, like: String, right: ValueExpression)
    extends LogicalExpression
case class AndLogicalExpression(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression
case class OrLogicalExpression(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression
case class NotLogicalExpression(expr: LogicalExpression) extends LogicalExpression
case class ExistsLogicalExpression(tuples: TupleCollectionExpression) extends LogicalExpression

case class Ident(name: String) extends Positional

case class ColumnSpec(name: Ident, typepos: Position, typ: Option[String])
case class ColumnDef(name: Ident,
                     typepos: Position,
                     typ: Type,
                     pkpos: Position,
                     fkr: Ident,
                     fkc: Ident,
                     unmarkable: Boolean,
                     auto: Boolean)
