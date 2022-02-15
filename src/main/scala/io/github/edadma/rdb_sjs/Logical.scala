package io.github.edadma.rdb_sjs

object Logical {

  def fromBoolean(b: Boolean): Logical = if (b) TRUE else FALSE

  def fromBooleanMaybeNegated(b: Boolean, negated: Boolean): Logical = fromBoolean(b ^ negated)
}

abstract class Logical {

  def unary_! : Logical

  def &&(that: => Logical): Logical

  def ||(that: => Logical): Logical

}

abstract class Mark(name: String) {

  def scala(that: Any): Mark

  def comparison(that: Any): Logical

  override def toString: String = name
}
