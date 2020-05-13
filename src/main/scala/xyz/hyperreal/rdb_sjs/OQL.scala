package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.JSON

class OQL(erd: String, conn: Connection) {

  private val model = ERModel(erd)

  def toJS(res: List[Map[String, Any]]) =
    res.map(_.toJSDictionary).toJSArray

  def pretty(res: List[Map[String, Any]]) =
    JSON.stringify(toJS(res), null.asInstanceOf[js.Array[js.Any]], 2)

  def query(s: String) = {
    val OQLQuery(resource, project, select, order, group) =
      OQLParser.parseQuery(s)

    val entity = model.get(resource.name, resource.pos)
    val projectbuf = new ListBuffer[(String, String)]
    val joinbuf = new ListBuffer[String]
    val graph: ProjectionBranch =
      if (project isDefined) {
        null
      } else {
        branches(resource.name, resource.pos, projectbuf, joinbuf)
      }

    val sql = new StringBuilder

    sql append s"SELECT ${projectbuf map { case (e, f) => s"$e.$f" } mkString (", ")}\n"
    sql append s"  FROM ${resource.name}"

    println(sql)
    //    val res =
    //      conn
    //        .executeSQLStatement(sql.toString)
    //        .asInstanceOf[RelationResult]
    //        .relation
    //        .collect
    //
    //    res.toList map (build(_, res.metadata, graph))

  }

  private def branches(entity: String,
                       pos: Position,
                       projectbuf: ListBuffer[(String, String)],
                       joinbuf: ListBuffer[String]): ObjectProjectionBranch = {
    ObjectProjectionBranch(model.list(entity, pos) map {
      case (field, attr: PrimitiveEntityAttribute) =>
        projectbuf += (entity -> field)
        PrimitiveProjectionNode(entity, field, attr)
      case (field, attr: ObjectEntityAttribute) =>
        joinbuf += entity
        EntityProjectionNode(
          entity,
          field,
          branches(attr.entityType, pos, projectbuf, joinbuf))
    })
  }

  private def build(row: Tuple, md: Metadata, branch: ProjectionBranch) = {
    def build(branch: ProjectionBranch): Map[String, Any] =
      branch match {
        //        case LiftedProjectionBranch(subfield) =>
        case ObjectProjectionBranch(fields) =>
          val obj = new mutable.LinkedHashMap[String, Any]

          for (f <- fields)
            f match {
              case EntityProjectionNode(table, field, branch) =>
                obj(field) = build(branch)
              case PrimitiveProjectionNode(table, field, typ) =>
                obj(field) = row(md.tableColumnMap(table, field))
            }

          obj.toMap
      }

    build(branch)
  }

  abstract class ProjectionNode { val table: String; val field: String }
  case class PrimitiveProjectionNode(table: String,
                                     field: String,
                                     typ: EntityAttribute)
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
