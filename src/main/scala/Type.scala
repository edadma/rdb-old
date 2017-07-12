package xyz.hyperreal.rdb


object Type {

	val names =
		Map(
			"logical" -> LogicalType,
			"integer" -> IntegerType,
			"float" -> FloatType,
			"string" -> StringType
		)

	def fromValue( v: Any ) = {
		def _fromValue: PartialFunction[Any, Type] = {
			case _: Logical => LogicalType
			case _: Int => IntegerType
			case _: Double => FloatType
			case _: String => StringType
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

//case object ShortType extends PrimitiveType
case object IntegerType extends PrimitiveType( "integer" ) with OrderedNumericalType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: java.lang.Integer, y: java.lang.Integer) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}

//case object LongType extends PrimitiveType
//case object BigintType extends PrimitiveType
case object FloatType extends PrimitiveType( "float" ) with OrderedNumericalType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: java.lang.Double, y: java.lang.Double) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}


//case object DecimalType extends PrimitiveType
//case object RationalType extends PrimitiveType
//case object ComplexIntegerType extends PrimitiveType
//case object ComplexBigintType extends PrimitiveType
//case object ComplexFloatType extends PrimitiveType
//case object ComplexRationalType extends PrimitiveType
//case object ComplexDecimalType extends PrimitiveType
//case object NumberType extends PrimitiveType
case object StringType extends PrimitiveType( "string" ) with OrderedType {

	def compare( x: AnyRef, y: AnyRef ) =
		(x, y) match {
			case (x: String, y: String) => x compareTo y
			case _ => sys.error( s"incomparable values: $x, $y" )
		}

}


//case object TextType extends PrimitiveType
//case object BinaryType extends PrimitiveType
//case object BlobType extends PrimitiveType
//case object TimestampType extends PrimitiveType
//case object TimestamptzType extends PrimitiveType
//case object DateType extends PrimitiveType
//case object DateTimeType extends PrimitiveType
//case object TimeType extends PrimitiveType
//case object TimeIntervalType extends PrimitiveType
//case object UUIDType extends PrimitiveType

//case class EnumeratedType( elements: List[String] ) extends SimpleType {
//	val name = "enum(" + elements.mkString( "," ) + ")"
//}

//case class SetType( elements: List[String] ) extends SimpleType {
//	val name = "set(" + elements.mkString( "," ) + ")"
//}

//case class ArrayType( parameter: SimpleType ) extends Type {
//	val name = s"array($parameter)"
//}
