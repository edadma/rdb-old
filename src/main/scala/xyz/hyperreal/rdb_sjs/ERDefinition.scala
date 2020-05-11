package xyz.hyperreal.rdb_sjs

import scala.collection.mutable

object ERDefinition {

  def apply(defn: String) = {
    val d = ERDParser.parseDefinition(defn)

  }

  private[rdb_sjs] abstract class EntityType
  private[rdb_sjs] case class SimpleEntityType(name: String)

}

class ERDefinition(
    entities: Map[String, Map[String, ERDefinition.EntityType]]) {

  import ERDefinition._

}
