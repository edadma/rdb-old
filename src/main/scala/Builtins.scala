package xyz.hyperreal.rdb_sjs

import math.Pi

object Builtins {

  val aggregateFunctions =
    List(
      "count" -> CountAggregateFunction,
      "sum" -> SumAggregateFunction,
      "avg" -> AvgAggregateFunction,
      "min" -> MinAggregateFunction,
      "max" -> MaxAggregateFunction,
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
