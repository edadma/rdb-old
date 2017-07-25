package xyz.hyperreal.rdb

import java.util.UUID
import java.time.{Instant, LocalDate}

import xyz.hyperreal.numbers.ComplexBigInt


object Type {

	val names =
		Map(
			"logical" -> LogicalType,
			"integer" -> IntegerType,
			"smallint" -> SmallintType,
			"float" -> FloatType,
			"text" -> TextType,
			"decimal" -> DecimalType,
			"date" -> DateType,
			"instant" -> InstantType
		)

	def fromValue( v: Any ) = {
		def _fromValue: PartialFunction[Any, Type] = {
			case _: Logical => LogicalType
			case _: Int => IntegerType
			case _: Double => FloatType
			case _: ComplexBigInt => ComplexIntegerType
			case _: String => TextType
			case _: LocalDate => DateType
			case _: Instant => InstantType
		}

		if (_fromValue isDefinedAt v)
			Some( _fromValue( v ) )
		else
			None
	}

}

trait Type extends Ordering[AnyRef]

trait SimpleType extends Type

abstract class PrimitiveType( val name: String ) extends SimpleType {
	override def toString = name
}

trait Auto {

	def next( v: AnyRef ): AnyRef

	def default: AnyRef

}

trait NumericalType extends Type

trait OrderedType extends Type

trait OrderedNumericalType extends NumericalType with OrderedType

case object LogicalType extends PrimitiveType( "logical" ) {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (java.lang.Boolean.TRUE, java.lang.Boolean.TRUE)|(java.lang.Boolean.FALSE, java.lang.Boolean.FALSE) => 0
			case (java.lang.Boolean.FALSE, java.lang.Boolean.TRUE) => 1
			case (java.lang.Boolean.TRUE, java.lang.Boolean.FALSE) => -1
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case object ByteType extends PrimitiveType( "byte" ) with NumericalType {
//
//	def compare( x: AnyRef, y: AnyRef ) =
//		(x, y) match {
//			case (x: java.lang.Byte, y: java.lang.Byte) => x compareTo y
//		}
//
//}

case object SmallintType extends PrimitiveType( "smallint" ) with OrderedNumericalType with Auto {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: java.lang.Short, y: java.lang.Short) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

	def next( v: AnyRef ) = (v.asInstanceOf[Short] + 1).asInstanceOf[java.lang.Short]

	def default = 1.asInstanceOf[java.lang.Short]

}

case object IntegerType extends PrimitiveType( "integer" ) with OrderedNumericalType with Auto {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: java.lang.Integer, y: java.lang.Integer) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

	def next( v: AnyRef ) = (v.asInstanceOf[Int] + 1).asInstanceOf[java.lang.Integer]

	def default = 1.asInstanceOf[java.lang.Integer]

}

//case object BigintType extends PrimitiveType
case object FloatType extends PrimitiveType( "float" ) with OrderedNumericalType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: java.lang.Double, y: java.lang.Double) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

case object DecimalType extends PrimitiveType( "decimal" ) with OrderedNumericalType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: BigDecimal, y: BigDecimal) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case object RationalType extends PrimitiveType
case object ComplexIntegerType extends PrimitiveType( "complex integer" ) with NumericalType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: ComplexBigInt, y: ComplexBigInt) =>
				x.re compareTo y.re match {
					case 0 => x.im compareTo y.im
					case c => c
				}
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}
//case object ComplexFloatType extends PrimitiveType
//case object ComplexRationalType extends PrimitiveType
//case object ComplexDecimalType extends PrimitiveType
//case object NumberType extends PrimitiveType
case object TextType extends PrimitiveType( "string" ) with OrderedType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: String, y: String) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case object TextType extends PrimitiveType
//case object BinaryType extends PrimitiveType
//case object BlobType extends PrimitiveType

case object DateType extends PrimitiveType( "date" ) with OrderedType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: LocalDate, y: LocalDate) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

case object InstantType extends PrimitiveType( "instant" ) with OrderedType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: Instant, y: Instant) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case object TimeIntervalType extends PrimitiveType
case object UUIDType extends PrimitiveType( "uuid" ) {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: UUID, y: UUID) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case class EnumeratedType( elements: List[String] ) extends SimpleType {
//	val name = "enum(" + elements.mkString( "," ) + ")"
//}

//case class SetType( elements: List[String] ) extends SimpleType {
//	val name = "set(" + elements.mkString( "," ) + ")"
//}

//case class ArrayType( parameter: SimpleType ) extends Type {
//	val name = s"array($parameter)"
//}
