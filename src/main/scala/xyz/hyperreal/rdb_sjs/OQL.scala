package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

class OQL(erd: String, conn: Connection) {

  private val model = ERModel(erd)

  def query(s: String) = {
    val OQLQuery(resource, project, select, order, group) =
      OQLParser.parseQuery(s)

    val entity = model.get(resource.name, resource.pos)
    val projectbuf = new ListBuffer[String]
    val graph: ProjectionBranch =
      if (project isDefined) {
        null
      } else {
        branches(resource.name, resource.pos, projectbuf)
      }

    val sql = new StringBuilder

    sql append s"SELECT ${projectbuf.mkString(", ")}\n"
    sql append s"  FROM ${resource.name}"

    val res =
      conn
        .executeSQLStatement(sql.toString)
        .asInstanceOf[RelationResult]
        .relation
        .collect

    for (r <- res)
      yield {
        build(r, res.metadata, graph)
      }
  }

  private def build(row: Tuple, md: Metadata, branch: ProjectionBranch) = {
    def build(branch: ProjectionBranch) =
      branch match {
//        case LiftedProjectionBranch(subfield) =>
        case ObjectProjectionBranch(fields) =>
          val obj = new mutable.HashMap[String, Any]

          for (f <- fields)
            f match {
//              case EntityProjectionNode(table, field, branch) =>
              case PrimitiveProjectionNode(table, field, typ) =>
                obj(field) = row(md.tableColumnMap(table, field))
            }
      }
  }

//model.list(resource.name, resource.pos)
  private def branches(entity: String,
                       pos: Position,
                       project: ListBuffer[String]) = {
    ObjectProjectionBranch(model.list(entity, pos) map {
      case (field, typ: PrimitiveEntityType) =>
        project += field
        PrimitiveProjectionNode(entity, field, typ)
    })
  }

  abstract class ProjectionNode { val table: String; val field: String }
  case class PrimitiveProjectionNode(table: String,
                                     field: String,
                                     typ: EntityType)
      extends ProjectionNode
  case class EntityProjectionNode(table: String,
                                  field: String,
                                  branch: ProjectionBranch)
      extends ProjectionNode

  abstract class ProjectionBranch
  case class ObjectProjectionBranch(fields: Seq[ProjectionNode])
      extends ProjectionBranch
  case class LiftedProjectionBranch(subfield: ProjectionNode)
      extends ProjectionBranch

}
