package xyz.hyperreal.rdb

import math.Pi

import xyz.hyperreal.lia.Math


trait ScalarFunction extends (List[Any] => Any) {

	def name: String

	def typ( inputs: List[Type] ): Type

	def apply( args: List[Any] ): Any

}

abstract class AbstractScalarFunction( val name: String ) extends ScalarFunction {

	def typ( inputs: List[Type] ) = FloatType

	override def toString = s"<scalar function '$name'>"

}

object PiScalarFunction extends AbstractScalarFunction( "pi" ) {
	def apply( args: List[Any] ) = Pi
}

object FloatScalarFunction extends AbstractScalarFunction( "float" ) {
	def apply( args: List[Any] ) =
		args match {
			case List( a: Number ) => a.doubleValue
		}
}

object AbsScalarFunction extends AbstractScalarFunction( "abs" ) {
	def apply( args: List[Any] ) =
		args match {
			case List( a ) => Math.absFunction( a )
		}
}

object sqrtFunction extends AbstractScalarFunction( "sqrt" ) {
	def apply( args: List[Any] ) =
		args match {
			case List( a ) => Math.sqrtFunction( a )
		}
}
