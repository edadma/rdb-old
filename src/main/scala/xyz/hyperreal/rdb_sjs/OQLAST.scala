package xyz.hyperreal.rdb_sjs

abstract class OQLAST extends AST

case class OQLQuery(resource: Ident,
                    select: Option[ExpressionOQL],
                    project: Option[ProjectOQL],
                    order: Option[List[(ExpressionOQL, Boolean)]],
                    group: Option[List[ExpressionOQL]])

abstract class ExpressionOQL extends OQLAST

case class VariableExpressionOQL(ids: List[Ident]) extends ExpressionOQL

case class ProjectOQL(lift: Boolean, props: List[ExpressionOQL])
    extends ExpressionOQL

case class ProjectionExpressionOQL(id: Ident, project: ProjectOQL)
    extends ExpressionOQL
