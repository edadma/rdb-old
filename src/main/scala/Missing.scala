package xyz.hyperreal.rdb


abstract class Logical {

	def not: Logical

	def and( that: Logical ): Logical

	def or( that: Logical ): Logical

}

abstract class Mark {

	def scala( that: AnyRef ): Mark

	def comparison( that: AnyRef ): Logical

}