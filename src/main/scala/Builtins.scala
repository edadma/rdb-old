package xyz.hyperreal.rdb

import math.Pi

import xyz.hyperreal.numbers.ComplexBigInt.i

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
			"sqrt" -> sqrtFunction
		)
	val constants =
		List(
			"pi" -> Pi,
			"i" -> i
		).asInstanceOf[List[(String, AnyRef)]]

}