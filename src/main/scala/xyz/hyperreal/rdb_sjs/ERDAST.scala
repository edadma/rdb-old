package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.Positional

abstract class ERDAST
case class DefinitionERD(blocks: List[BlockERD]) extends ERDAST

abstract class BlockERD extends ERDAST
case class TypeBlockERD(name: Ident,
                        underlying: Ident,
                        condition: ExpressionERD)
    extends BlockERD

abstract class ExpressionERD extends ERDAST with Positional

case class VariableExpressionERD(name: Ident) extends ExpressionERD
case class FloatLiteralERD(n: String) extends ExpressionERD
case class IntegerLiteralERD(n: String) extends ExpressionERD
case class StringLiteralERD(s: String) extends ExpressionERD
case class NotExpressionERD(expr: ExpressionERD) extends ExpressionERD
case class ComparisonExpressionERD(first: ExpressionERD,
                                   comps: List[(String, ExpressionERD)])
    extends ExpressionERD
case class AndExpressionERD(left: ExpressionERD, right: ExpressionERD)
    extends ExpressionERD
case class OrExpressionERD(left: ExpressionERD, right: ExpressionERD)
    extends ExpressionERD

abstract class TypeSpecifierERD extends ERDAST with Positional
case class SimpleTypeERD(name: Ident) extends TypeSpecifierERD

case class EntityBlockERD(name: Ident, fields: List[EntityFieldERD])
    extends BlockERD
case class EntityFieldERD(name: Ident, typ: TypeSpecifierERD, pk: Boolean)
    extends ERDAST
