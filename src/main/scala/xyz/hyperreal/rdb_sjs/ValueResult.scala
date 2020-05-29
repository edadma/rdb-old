package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.Position

trait ValueResult {
  val pos: Position
  val table: String
  val heading: String
  val typ: Type
}

case class LiteralValue(pos: Position, table: String, heading: String, typ: Type, value: Any) extends ValueResult
case class VariableValue(pos: Position, table: String, heading: String, typ: Type, value: Any) extends ValueResult
case class FieldValue(pos: Position, table: String, heading: String, typ: Type, index: Int, depth: Int)
    extends ValueResult
case class MarkedValue(pos: Position, table: String, heading: String, typ: Type, m: Mark) extends ValueResult
case class BinaryValue(pos: Position,
                       table: String,
                       heading: String,
                       typ: Type,
                       left: ValueResult,
                       operation: String,
                       right: ValueResult)
    extends ValueResult
case class CaseValue(pos: Position,
                     table: String,
                     heading: String,
                     typ: Type,
                     whens: List[(LogicalResult, ValueResult)],
                     els: Option[ValueResult])
    extends ValueResult
case class AggregateFunctionValue(pos: Position,
                                  table: String,
                                  heading: String,
                                  typ: Type,
                                  af: AggregateFunction,
                                  args: List[ValueResult])
    extends ValueResult {
  var func: AggregateFunctionInstance = _
}
case class ScalarFunctionValue(pos: Position,
                               table: String,
                               heading: String,
                               typ: Type,
                               func: ScalarFunction,
                               args: List[ValueResult])
    extends ValueResult
case class UnaryValue(pos: Position, table: String, heading: String, typ: Type, v: ValueResult, operation: String)
    extends ValueResult
case class LogicalValue(pos: Position, table: String, heading: String, typ: Type, logical: LogicalResult)
    extends ValueResult
case class AliasValue(pos: Position, table: String, heading: String, typ: Type, apos: Position, v: ValueResult)
    extends ValueResult
