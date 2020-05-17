package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.util.parsing.input.Position

class ERModel(defn: String) {

  private val entities: Map[String, Entity] = {
    val d = ERDParser.parseDefinition(defn)
    val m = new mutable.HashMap[String, Entity]

    d.blocks foreach {
      case EntityBlockERD(entity, fields) =>
        if (m contains entity.name)
          problem(entity.pos, s"entity '${entity.name}' already defined")
        else {
          var epk: String = null
          val columns = new mutable.HashSet[String]
          var attrs = Map.empty[String, EntityAttribute]

          for (EntityFieldERD(attr, column, typ, pk) <- fields) {
            if (columns(column.name))
              problem(
                attr.pos,
                s"column '${column.name}' already exists for this entity'")
            else if (attrs contains attr.name)
              problem(
                attr.pos,
                s"attribute '${attr.name}' already exists for this entity'")
            else {
              val fieldtype =
                typ match {
                  case SimpleTypeERD(typ) =>
                    m get typ.name match {
                      case Some(e) =>
                        ObjectEntityAttribute(column.name, typ.name, e)
                      case None =>
                        PrimitiveEntityAttribute(column.name, typ.name)
                    }
                }

              attrs += (attr.name -> fieldtype)
              columns += column.name
            }
            if (pk) {
              if (epk ne null)
                problem(
                  attr.pos,
                  "there is already a primary key defined for this entity")
              else
                epk = column.name
            }
          }

          m(entity.name) = Entity(if (epk ne null) Some(epk) else None, attrs)
        }
      case TypeBlockERD(name, underlying, condition) =>
    }
    m.toMap
  }

  def get(table: String, pos: Position): Entity =
    entities get table match {
      case None    => problem(pos, s"unknown resource: '$table'")
      case Some(e) => e
    }

  def list(table: String, pos: Position): Seq[(String, EntityAttribute)] =
    get(table, pos).attributes.toList

}

case class Entity(pk: Option[String], attributes: Map[String, EntityAttribute])

abstract class EntityAttribute
case class PrimitiveEntityAttribute(column: String, primitiveType: String)
    extends EntityAttribute
case class ObjectEntityAttribute(column: String,
                                 entityType: String,
                                 entity: Entity)
    extends EntityAttribute
