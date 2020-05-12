package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.util.parsing.input.Position

object ERModel {

  def apply(defn: String): ERModel = {
    val d = ERDParser.parseDefinition(defn)
    val m = new mutable.HashMap[String, Entity]

    d.blocks foreach {
      case EntityBlockERD(entity, fields) =>
        if (m contains entity.name)
          problem(entity.pos, s"entity '${entity.name}' already defined")
        else {
          var epk: String = null
          var e = Map.empty[String, EntityType]

          for (EntityFieldERD(field, typ, pk) <- fields) {
            if (e contains field.name)
              problem(field.pos,
                      s"field '${field.name}' already exists in this entity'")
            else {
              val fieldtype =
                typ match {
                  case SimpleTypeERD(typ) => PrimitiveEntityType(typ.name)
                }

              e += (field.name -> fieldtype)
            }
            if (pk) {
              if (epk ne null)
                problem(
                  field.pos,
                  "there is already a primary key defined for this entity")
              else
                epk = field.name
            }
          }

          m(entity.name) = Entity(if (epk ne null) Some(epk) else None, e)
        }
      case TypeBlockERD(name, underlying, condition) =>
    }
    new ERModel(m.toMap)
  }

}

class ERModel(entities: Map[String, Entity]) {

  def get(table: String, pos: Position): Entity =
    entities get table match {
      case None    => problem(pos, s"unknown resource: '$table'")
      case Some(e) => e
    }

  def list(table: String, pos: Position): Seq[(String, EntityType)] =
    get(table, pos).fields.toList

}

case class Entity(pk: Option[String], fields: Map[String, EntityType])

abstract class EntityType
case class PrimitiveEntityType(name: String) extends EntityType
