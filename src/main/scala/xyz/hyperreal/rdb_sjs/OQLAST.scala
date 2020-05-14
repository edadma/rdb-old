package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.Positional

abstract class OQLAST extends AST

case class OQLQuery(resource: Ident,
                    project: ProjectExpressionOQL,
                    select: Option[ExpressionOQL],
                    order: Option[List[(ExpressionOQL, Boolean)]],
                    group: Option[List[ExpressionOQL]])

abstract class ExpressionOQL extends OQLAST with Positional

case class VariableExpressionOQL(ids: List[Ident]) extends ExpressionOQL

case class InfixExpressionOQL(left: ExpressionOQL,
                              op: String,
                              right: ExpressionOQL)
    extends ExpressionOQL
case class PrefixExpressionOQL(op: String, expr: ExpressionOQL)
    extends ExpressionOQL
case class GroupedExpressionOQL(expr: ExpressionOQL) extends ExpressionOQL
case class FloatLiteralOQL(n: String) extends ExpressionOQL
case class IntegerLiteralOQL(n: String) extends ExpressionOQL
case class StringLiteralOQL(s: String) extends ExpressionOQL

abstract class ProjectExpressionOQL
case class ProjectAttributesOQL(lift: Boolean, attrs: List[AttributeOQL])
    extends ProjectExpressionOQL
case object ProjectAllOQL extends ProjectExpressionOQL

case class AttributeOQL(attr: Ident, project: ProjectExpressionOQL)
