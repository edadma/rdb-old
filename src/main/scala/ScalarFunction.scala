package xyz.hyperreal.rdb

import math.Pi

import xyz.hyperreal.lia.Math


trait ScalarFunction extends (List[AnyRef] => AnyRef) {

	def name: String

	def typ( inputs: List[Type] ): Type

	def apply( args: List[AnyRef] ): AnyRef

}

abstract class AbstractScalarFunction( val name: String ) extends ScalarFunction {

	def typ( inputs: List[Type] ) = FloatType

	override def toString = s"<scalar function '$name'>"

}

object BuiltinScalarFunctions {

	val piFunction =
		new AbstractScalarFunction( "pi" ) {
			def apply( args: List[AnyRef] ) = Pi.asInstanceOf[Number]
		}
	val absFunction =
		new AbstractScalarFunction( "abs" ) {
			def apply( args: List[AnyRef] ) =
				args match {
					case List( a ) => Math.absFunction( a )
				}
		}
	val sqrtFunction =
		new AbstractScalarFunction( "sqrt" ) {
			def apply( args: List[AnyRef] ) =
				args match {
					case List( a ) => Math.sqrtFunction( a )
				}
		}
}