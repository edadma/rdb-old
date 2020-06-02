package xyz.hyperreal.rdb_sjs

import math._

object Builtins {

  val aggregateFunctions =
    List(
      "COUNT" -> CountAggregateFunction,
      "count" -> CountAggregateFunction,
      "SUM" -> SumAggregateFunction,
      "sum" -> SumAggregateFunction,
      "AVG" -> AvgAggregateFunction,
      "avg" -> AvgAggregateFunction,
      "MIN" -> MinAggregateFunction,
      "min" -> MinAggregateFunction,
      "MAX" -> MaxAggregateFunction,
      "max" -> MaxAggregateFunction,
      "LIST" -> ListAggregateFunction,
      "list" -> ListAggregateFunction
    )
  val scalarFunctions =
    List(
      "CONCAT" -> ConcatScalarFunction,
      "concat" -> ConcatScalarFunction,
      "LOWER" -> LowerScalarFunction,
      "lower" -> LowerScalarFunction,
      "UPPER" -> UpperScalarFunction,
      "upper" -> UpperScalarFunction,
      "FLOAT" -> FloatScalarFunction,
      "float" -> FloatScalarFunction,
      "ABS" -> AbsScalarFunction,
      "abs" -> AbsScalarFunction,
      "SQRT" -> sqrtScalarFunction,
      "sqrt" -> sqrtScalarFunction
    )
  val constants =
    List(
      "PI" -> Pi,
      "pi" -> Pi
    )

}
