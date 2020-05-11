package xyz.hyperreal.rdb_sjs

import scala.collection.mutable

object ERDefinition {

  def apply(defn: String) = {
    val d = ERDParser.parseDefinition(defn)
    val m = new mutable.HashMap[String, Entity]

    for (b <- d.blocks)
      b match {
        case EntityBlockERD(entity, fields) =>
          if (m contains entity.name)
            problem(entity.pos, s"entity '${entity.name}' already defined")
          else {
            var epk: String = null
            var e = Map.empty[String, ERDefinition.EntityType]

            for (EntityFieldERD(field, typ, pk) <- fields) {
              if (e contains field.name)
                problem(field.pos,
                        s"field '${field.name}' already exists in this entity'")
              else {
                val fieldtype =
                  typ match {
                    case SimpleTypeERD(typ) => SimpleEntityType(typ.name)
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
    m.toMap
  }

  private[rdb_sjs] case class Entity(pk: Option[String],
                                     fields: Map[String, EntityType])
  private[rdb_sjs] abstract class EntityType
  private[rdb_sjs] case class SimpleEntityType(name: String) extends EntityType

}

class ERDefinition(entities: Map[String, ERDefinition.Entity]) {

  import ERDefinition._

}
