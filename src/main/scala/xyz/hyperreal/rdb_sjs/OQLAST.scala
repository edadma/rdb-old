package xyz.hyperreal.rdb_sjs

abstract class OQLAST extends AST

case class OQLQuery(resource: Ident,
                    project: ProjectExpressionOQL,
                    select: Option[ExpressionOQL],
                    order: Option[List[(ExpressionOQL, Boolean)]],
                    group: Option[List[ExpressionOQL]])

abstract class ExpressionOQL extends OQLAST

case class VariableExpressionOQL(ids: List[Ident]) extends ExpressionOQL

abstract class ProjectExpressionOQL
case class ProjectAttributesOQL(lift: Boolean, attrs: List[AttributeOQL])
    extends ProjectExpressionOQL
case object ProjectAllOQL extends ProjectExpressionOQL

case class AttributeOQL(attr: Ident, project: ProjectExpressionOQL)
