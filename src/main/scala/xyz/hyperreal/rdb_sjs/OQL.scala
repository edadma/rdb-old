package xyz.hyperreal.rdb_sjs

class OQL(model: String) {

  private val erd = ERDefinition(model)

  def query(s: String) = {
    val OQLQuery(resource, select, project, order, group) =
      OQLParser.parseQuery(s)

    erd get resource.name match {
      case None =>
        problem(resource.pos, s"unknown resource: '${resource.name}'")
      case Some(entity) =>
    }
  }

}
