package xyz.hyperreal.rdb


abstract class AbstractRelation {

	def header: Seq[Column]

	def columnMap: Map[String, Int]

	def iterator: Iterator[Vector[AnyRef]]

	def foreach( f: Vector[AnyRef] => Unit ): Unit = {
		for (row <- iterator)
			f( row )
	}

}

object Type {



}

trait Type

trait SimpleType extends Type

trait PrimitiveType extends SimpleType
case object ByteType extends PrimitiveType
case object ShortType extends PrimitiveType
case object IntegerType extends PrimitiveType
case object LongType extends PrimitiveType
case object BigintType extends PrimitiveType
case object FloatType extends PrimitiveType
case object DecimalType extends PrimitiveType
case object RationalType extends PrimitiveType
case object ComplexIntegerType extends PrimitiveType
case object ComplexBigintType extends PrimitiveType
case object ComplexFloatType extends PrimitiveType
case object ComplexRationalType extends PrimitiveType
case object ComplexDecimalType extends PrimitiveType
case object NumberType extends PrimitiveType
case object StringType extends PrimitiveType
case object TextType extends PrimitiveType
case object BinaryType extends PrimitiveType
case object BlobType extends PrimitiveType
case object TimestampType extends PrimitiveType
case object TimestamptzType extends PrimitiveType
case object DateType extends PrimitiveType
case object DateTimeType extends PrimitiveType
case object TimeType extends PrimitiveType
case object TimeIntervalType extends PrimitiveType
case object UUIDType extends PrimitiveType

case class EnumeratedType( elements: List[String] ) extends SimpleType

case class SetType( elements: List[String] ) extends SimpleType

case class ArrayType( parameter: SimpleType ) extends Type

case class Column( name: String, typ: Type )