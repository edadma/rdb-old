package xyz.hyperreal.rdb_sjs

import math.Pi

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
      "float" -> FloatScalarFunction,
      "abs" -> AbsScalarFunction,
      "sqrt" -> sqrtScalarFunction
    )
  val constants =
    List(
      "pi" -> Pi
    ).asInstanceOf[List[(String, Any)]]

}
