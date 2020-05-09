package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.{Positional, Position}

abstract class OQLAST extends AST

case class OQLQuery(resource: Ident, select: ExpressionOQL, project: ProjectOQL))

abstract class ExpressionOQL extends OQLAST

case class VariableExpressionOQL(ids: List[Ident]) extends ExpressionOQL
