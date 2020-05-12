package xyz.hyperreal.rdb_sjs

class OQL(model: String) {

  private val erd = ERDefinition(model)

  def query(s: String) = {
    val OQLQuery(resource, project, select, order, group) =
      OQLParser.parseQuery(s)

    erd get resource.name match {
      case None =>
        problem(resource.pos, s"unknown resource: '${resource.name}'")
      case Some(entity) =>
        if (project isDefined) {} else {
          ObjectProjectionNode(resource.name, )
        }
    }
  }

  abstract class ProjectionNode { val table: String; val field: String }
  case class ObjectProjectionNode(table: String,
                                  field: String,
                                  projection: List[ProjectionNode])
      extends ProjectionNode
  case class LiftedProjectionNode(table: String,
                                  field: String,
                                  subfield: ProjectionNode)
      extends ProjectionNode

}
