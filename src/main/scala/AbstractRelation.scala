package xyz.hyperreal.rdb

import scala.util.parsing.input.Position


trait Relation {

	def header: IndexedSeq[Column]

	def iterator: Iterator[Vector[AnyRef]]

	def foreach( f: Vector[AnyRef] => Unit ): Unit

	def size: Int

	lazy val columnNameMap = (header map (_.name) zipWithIndex) toMap

	lazy val columnMap = header.zipWithIndex toMap

	lazy val headerSet = header toSet

}

case class Column( name: String, typ: Type )

abstract class AbstractRelation extends Relation {

	def foreach( f: Vector[AnyRef] => Unit ) =
		for (row <- iterator)
			f( row )

}

object Type {

	def fromSpec( pos: Position, spec: String ) = {

		spec match {
			case "integer" => IntegerType
			case "float" => FloatType
			case "string" => StringType
			case _ => problem( pos, s"unrecognized type name '$spec'" )
		}
	}

}

trait Type

trait SimpleType extends Type

abstract class PrimitiveType( val name: String ) extends SimpleType {
	override def toString = name
}

trait NumericalType extends Type

case object ByteType extends PrimitiveType( "byte" ) with NumericalType
//case object ShortType extends PrimitiveType
case object IntegerType extends PrimitiveType( "integer" ) with NumericalType
//case object LongType extends PrimitiveType
//case object BigintType extends PrimitiveType
case object FloatType extends PrimitiveType( "float" ) with NumericalType
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
