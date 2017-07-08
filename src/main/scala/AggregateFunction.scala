package xyz.hyperreal.rdb

import xyz.hyperreal.lia.Math

import collection.mutable.PriorityQueue


trait AggregateFunction {

	def name: String

	def typ( input: Type ): Type

	def next( args: List[AnyRef] ): Unit

	def result: AnyRef

}

abstract class AbstractAggregateFunction[T <: AnyRef ]( val name: String ) extends AggregateFunction {

	protected var intermediate: T = _
	protected var count = 0

	def compute( next: AnyRef ): T = null.asInstanceOf[T]

	def next( args: List[AnyRef] ): Unit = {
		count += 1
		intermediate = compute( args.head )
	}

	def result = intermediate

}

object AbstractAggregateFunction {

	val add = Math.lookup( '+ )
	val div = Math.lookup( '/ )
	val lt = Math.lookup( '< )

}

class CountAggregateFunction extends AbstractAggregateFunction[java.lang.Integer]( "count" ) {

	def typ( input: Type ) = IntegerType

	override def result = count

}

class SumAggregateFunction extends AbstractAggregateFunction[Number]( "sum" ) {

	def typ( input: Type ) = input

	override def compute( next: AnyRef ) = Math( AbstractAggregateFunction.add, intermediate, next ).asInstanceOf[Number]

}

class AvgAggregateFunction extends AbstractAggregateFunction[Number]( "avg" ) {

	def typ( input: Type ) = FloatType//todo: do this properly

	override def compute( next: AnyRef ) = Math( AbstractAggregateFunction.add, intermediate, next ).asInstanceOf[Number]

	override def result = Math( AbstractAggregateFunction.div, intermediate, count ).asInstanceOf[Number]

}

class MinAggregateFunction extends AbstractAggregateFunction[Number]( "min" ) {

	def typ( input: Type ) = input

	override def compute( next: AnyRef ) = {
		if (intermediate eq null)
			next.asInstanceOf[Number]
		else
			if (Math.predicate( AbstractAggregateFunction.lt, next, intermediate ))
				next.asInstanceOf[Number]
			else
				intermediate
	}

}

class MaxAggregateFunction extends AbstractAggregateFunction[Number]( "max" ) {

	def typ( input: Type ) = input

	override def compute( next: AnyRef ) = {
		if (intermediate eq null)
			next.asInstanceOf[Number]
		else
		if (Math.predicate( AbstractAggregateFunction.lt, intermediate, next ))
			next.asInstanceOf[Number]
		else
			intermediate
	}

}

class ListAggregateFunction extends AbstractAggregateFunction[String]( "list" ) {

	def typ( input: Type ) = StringType

	override def compute( next: AnyRef ) = if (intermediate eq null) next.toString else s"$intermediate, $next"

}

//class MedianAggregateFunction extends AbstractAggregateFunction[Number]( "median" ) {
//
//	val q = new PriorityQueue[Number].reverse
//
//	override def compute( next: AnyRef ) =
//
//}

// Mode