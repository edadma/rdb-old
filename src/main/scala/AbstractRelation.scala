package xyz.hyperreal.rdb


abstract class AbstractRelation {

	val columnMap = (header map (_.name) zipWithIndex) toMap

	def header: Seq[Column]

	def iterator: Iterator[Vector[AnyRef]]

	def foreach( f: Vector[AnyRef] => Unit ) =
		for (row <- iterator)
			f( row )

}

object Type {

	def fromSpec( spec: Ident ) = {
		val Ident( p, s ) = spec

		s match {
			case "integer" => IntegerType
			case "float" => FloatType
			case "string" => StringType
			case _ => problem( p, s"unrecognized type name '$s'" )
		}
	}

}

trait Type

trait SimpleType extends Type

abstract class PrimitiveType( val name: String ) extends SimpleType {
	override def toString = name
}

case object ByteType extends PrimitiveType( "byte" )
//case object ShortType extends PrimitiveType
case object IntegerType extends PrimitiveType( "integer" )
//case object LongType extends PrimitiveType
//case object BigintType extends PrimitiveType
case object FloatType extends PrimitiveType( "float" )
//case object DecimalType extends PrimitiveType
//case object RationalType extends PrimitiveType
//case object ComplexIntegerType extends PrimitiveType
//case object ComplexBigintType extends PrimitiveType
//case object ComplexFloatType extends PrimitiveType
//case object ComplexRationalType extends PrimitiveType
//case object ComplexDecimalType extends PrimitiveType
//case object NumberType extends PrimitiveType
case object StringType extends PrimitiveType( "string" )
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

case class EnumeratedType( elements: List[String] ) extends SimpleType {
	val name = "enum(" + elements.mkString( "," ) + ")"
}

case class SetType( elements: List[String] ) extends SimpleType {
	val name = "set(" + elements.mkString( "," ) + ")"
}

case class ArrayType( parameter: SimpleType ) extends Type {
	val name = s"array($parameter)"
}

case class Column( name: String, typ: Type )