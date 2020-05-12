package xyz.hyperreal.rdb_sjs

import scala.util.parsing.input.Position

class OQL(erd: String) {

  private val model = ERModel(erd)

  def query(s: String) = {
    val OQLQuery(resource, project, select, order, group) =
      OQLParser.parseQuery(s)

    model get resource.name match {
      case None =>
        problem(resource.pos, s"unknown resource: '${resource.name}'")
      case Some(entity) =>
        if (project isDefined) {} else {
          ObjectProjectionBranch()
        }
    }
  }

  private def branches(entity: String, pos: Position) =
    model get entity match {
      case Some(value) =>
      case None        =>
    }

  abstract class ProjectionNode { val table: String; val field: String }
  case class DataProjectionNode(table: String, field: String)
      extends ProjectionNode
  case class EntityProjectionNode(table: String,
                                  field: String,
                                  branch: ProjectionBranch)
      extends ProjectionNode

  abstract class ProjectionBranch
  case class ObjectProjectionBranch(fields: List[ProjectionNode])
  case class LiftedProjectionBranch(subfield: ProjectionNode)
      extends ProjectionBranch

}
