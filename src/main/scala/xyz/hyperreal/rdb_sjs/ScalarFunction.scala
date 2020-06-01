package xyz.hyperreal.rdb_sjs

import xyz.hyperreal.dal_sjs.BasicDAL.{absFunction, sqrtFunction}

trait ScalarFunction extends (List[Any] => Any) {

  def name: String

  def typ(inputs: List[Type]): Type

  def apply(args: List[Any]): Any

}

abstract class AbstractScalarFunction(val name: String, val ret: Type) extends ScalarFunction {

  def typ(inputs: List[Type]) = ret

  override def toString = s"<scalar function '$name'>"

}

object LowerScalarFunction extends AbstractScalarFunction("lower", FloatType) {
  def apply(args: List[Any]) =
    args match {
      case List(a: String) => a.toLowerCase
    }
}

object UpperScalarFunction extends AbstractScalarFunction("upper", FloatType) {
  def apply(args: List[Any]) =
    args match {
      case List(a: String) => a.toUpperCase
    }
}

object FloatScalarFunction extends AbstractScalarFunction("float", FloatType) {
  def apply(args: List[Any]) =
    args match {
      case List(a: Number) => a.doubleValue
    }
}

object AbsScalarFunction extends AbstractScalarFunction("abs", FloatType) {
  def apply(args: List[Any]) =
    args match {
      case List(a: Number) => absFunction(a)
    }
}

object sqrtScalarFunction extends AbstractScalarFunction("sqrt", FloatType) {
  def apply(args: List[Any]) =
    args match {
      case List(a: Number) => sqrtFunction(a)
    }
}
