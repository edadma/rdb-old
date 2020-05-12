package xyz.hyperreal.rdb_sjs

case class Entity(pk: Option[String], fields: Map[String, EntityType])

abstract class EntityType
case class SimpleEntityType(name: String) extends EntityType
