package xyz.hyperreal.rdb_sjs

abstract class OQLAST extends AST

case class OQLQuery(resource: Ident,
                    project: ProjectExpressionOQL,
                    select: Option[ExpressionOQL],
                    order: Option[List[(ExpressionOQL, Boolean)]],
                    group: Option[List[ExpressionOQL]])

abstract class ExpressionOQL extends OQLAST

case class VariableExpressionOQL(ids: List[Ident]) extends ExpressionOQL

case class AttributeOQL(v: Ident) extends ProjectExpressionOQL

abstract class ProjectExpressionOQL
case class ProjectFieldsOQL(props: List[ProjectExpressionOQL])
    extends ProjectExpressionOQL
case class ProjectLiftedOQL(props: ProjectExpressionOQL)
    extends ProjectExpressionOQL

case object ProjectAllOQL extends ProjectExpressionOQL

case class ProjectionExpressionOQL(id: Ident, project: ProjectExpressionOQL)
    extends ProjectExpressionOQL
