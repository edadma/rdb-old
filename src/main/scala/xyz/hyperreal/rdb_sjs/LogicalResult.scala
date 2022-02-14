package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.Position

trait LogicalResult {
  val heading: String
}

case class LiteralLogical(heading: String, value: Logical) extends LogicalResult
case class AndLogical(heading: String, left: LogicalResult, right: LogicalResult) extends LogicalResult
case class OrLogical(heading: String, left: LogicalResult, right: LogicalResult) extends LogicalResult
case class NotLogical(heading: String, expr: LogicalResult) extends LogicalResult
case class ComparisonLogical(heading: String, left: ValueResult, pos: Position, comp: String, right: ValueResult)
    extends LogicalResult
case class LikeLogical(heading: String,
                       pos: Position,
                       left: ValueResult,
                       right: ValueResult,
                       negated: Boolean,
                       casesensitive: Boolean)
    extends LogicalResult
case class IsNullLogical(heading: String, expr: ValueResult, negated: Boolean) extends LogicalResult
case class InLogical(heading: String, expr: ValueResult, negated: Boolean, list: List[ValueResult])
    extends LogicalResult
case class InQueryLogical(heading: String, expr: ValueResult, negated: Boolean, tuples: Iterable[Tuple])
    extends LogicalResult
case class ExistsLogical(heading: String, tuples: Iterable[Tuple]) extends LogicalResult
