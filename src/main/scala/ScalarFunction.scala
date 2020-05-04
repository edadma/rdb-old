package xyz.hyperreal.rdb_sjs

trait ScalarFunction extends (List[Any] => Any) {

  def name: String

  def typ(inputs: List[Type]): Type

  def apply(args: List[Any]): Any

}

abstract class AbstractScalarFunction(val name: String) extends ScalarFunction {

  def typ(inputs: List[Type]) = FloatType

  override def toString = s"<scalar function '$name'>"

}

object FloatScalarFunction extends AbstractScalarFunction("float") {
  def apply(args: List[Any]) =
    args match {
      case List(a: Number) => a.doubleValue
    }
}

object AbsScalarFunction extends AbstractScalarFunction("abs") {
  def apply(args: List[Any]) =
    args match {
      case List(a: BigDecimal) => a.abs
    }
}

object sqrtFunction extends AbstractScalarFunction("sqrt") {
  def apply(args: List[Any]) =
    args match {
      case List(a) => Math.sqrtFunction(a)
    }
}
