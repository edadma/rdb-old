package xyz.hyperreal.rdb_sjs

import java.time.{Instant, LocalDate}

import xyz.hyperreal.dal.BasicDAL.{compute, compare => dcompare}

object Type {

  val names =
    Map(
      "boolean" -> LogicalType,
      "int" -> IntegerType,
      "integer" -> IntegerType,
      "smallint" -> SmallintType,
      "bigint" -> BigintType,
      "float" -> FloatType,
      "text" -> TextType,
      "decimal" -> DecimalType,
      "date" -> DateType,
      "timestamp" -> TimestampType,
      "uuid" -> TextType // todo: implement uuid type
    )

  def fromValue(v: Any): Option[Type] = {
    def _fromValue: PartialFunction[Any, Type] = {
      case _: Logical    => LogicalType
      case _: Int        => IntegerType
      case _: Double     => FloatType
      case _: String     => TextType
      case _: BigDecimal => DecimalType
      case _: LocalDate  => DateType
      case _: Instant    => TimestampType
    }

    if (_fromValue isDefinedAt v)
      Some(_fromValue(v))
    else
      None
  }

}

abstract class Type extends Ordering[Any] {
  val name: String

  override def toString: String = name
}

abstract class PrimitiveType(val name: String) extends Type

abstract class NumericalType(name: String) extends PrimitiveType(name) {

  def compare(x: Any, y: Any): Int =
    (x, y) match {
      case (x: Number, y: Number) => dcompare(x, y)
      case _                      => sys.error(s"incomparable values: $x, $y")
    }

}

trait Auto {

  def next(v: Number): Number

  def default: Number

}

trait IntegralType extends Auto {

  def next(v: Number): Number = compute(Symbol("+"), v, 1)

  def default = 1

}

case object LogicalType extends PrimitiveType("boolean") {

  def compare(x: Any, y: Any): Int =
    (x, y) match {
      case (true, true) | (false, false) =>
        0
      case (false, true) => 1
      case (true, false) => -1
      case _             => sys.error(s"incomparable values: $x, $y")
    }

}

//case object ByteType extends PrimitiveType( "byte" ) with NumericalType {
//
//	def compare( x: Any, y: Any ) =
//		(x, y) match {
//			case (x: java.lang.Byte, y: java.lang.Byte) => x compareTo y
//		}
//
//}

case object SmallintType extends NumericalType("smallint") with IntegralType

case object IntegerType extends NumericalType("integer") with IntegralType

case object BigintType extends NumericalType("bigint") with IntegralType

case object FloatType extends NumericalType("float")

case object DecimalType extends NumericalType("decimal")

case object TextType extends PrimitiveType("string") {

  def compare(x: Any, y: Any) =
    (x, y) match {
      case (x: String, y: String) => x compare y
      case _                      => sys.error(s"incomparable values: $x, $y")
    }

}

//case object BinaryType extends PrimitiveType
//case object BlobType extends PrimitiveType

case object DateType extends PrimitiveType("date") {

  def compare(x: Any, y: Any) =
    (x, y) match {
      case (x: LocalDate, y: LocalDate) => x compareTo y
      case _                            => sys.error(s"incomparable values: $x, $y")
    }

}

case object TimestampType extends PrimitiveType("timestamp") {

  def compare(x: Any, y: Any) =
    (x, y) match {
      case (x: Instant, y: Instant) => x compareTo y
      case _                        => sys.error(s"incomparable values: $x, $y")
    }

}

//case object TimeIntervalType extends PrimitiveType
//case object UUIDType extends PrimitiveType("uuid") {
//
//  def compare(x: Any, y: Any) =
//    (x, y) match {
//      case (x: UUID, y: UUID) => x compareTo y
//      case _                  => sys.error(s"incomparable values: $x, $y")
//    }
//
//}

//case class EnumeratedType( elements: List[String] ) extends SimpleType {
//	val name = "enum(" + elements.mkString( "," ) + ")"
//}

//case class SetType( elements: List[String] ) extends SimpleType {
//	val name = "set(" + elements.mkString( "," ) + ")"
//}

//case class ArrayType( parameter: SimpleType ) extends Type {
//	val name = s"array($parameter)"
//}
