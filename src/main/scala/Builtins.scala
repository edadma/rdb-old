package xyz.hyperreal.rdb


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
			"pi" -> PiScalarFunction,
			"float" -> FloatScalarFunction,
			"abs" -> AbsScalarFunction,
			"sqrt" -> sqrtFunction
		)

}