package xyz.hyperreal.rdb_sjs

object Logical {

  def fromBoolean(b: Boolean) = if (b) TRUE else FALSE

}
abstract class Logical {

  def not: Logical

  def and(that: => Logical): Logical

  def or(that: => Logical): Logical

}

abstract class Mark(name: String) {

  def scala(that: Any): Mark

  def comparison(that: Any): Logical

  override def toString = name
}
