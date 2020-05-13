package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.JSON

class OQL(erd: String, conn: Connection) {

  private val model = ERModel(erd)

  def toJS(a: Any): js.Any =
    a match {
      case l: List[_] => l map toJS toJSArray
      case m: Map[_, _] =>
        (m map { case (k, v) => k -> toJS(v) })
          .asInstanceOf[Map[String, Any]]
          .toJSDictionary
      case _ => a.asInstanceOf[js.Any]
    }

  def pretty(res: List[Map[String, Any]]) =
    JSON.stringify(toJS(res), null.asInstanceOf[js.Array[js.Any]], 2)

  def query(s: String) = {
    val OQLQuery(resource, project, select, order, group) =
      OQLParser.parseQuery(s)

    val projectbuf = new ListBuffer[(String, String)]
    val joinbuf = new ListBuffer[(String, String, String, String, String)]
    val graph: ProjectionBranch =
      if (project isDefined) {
        null
      } else {
        branches(resource.name, resource.pos, projectbuf, joinbuf, Nil)
      }

    val sql = new StringBuilder

    sql append s"SELECT ${projectbuf map { case (e, f) => s"$e.$f" } mkString (", ")}\n"
    sql append s"  FROM ${resource.name}"

    for ((lt, lf, rt, rta, rf) <- joinbuf)
      sql append s" JOIN $rt AS $rta ON $lt.$lf = $rta.$rf"

    sql append '\n'

    print(sql)
    val res =
      conn
        .executeSQLStatement(sql.toString)
        .asInstanceOf[RelationResult]
        .relation
        .collect

    res.toList map (build(_, res.metadata, graph))
  }

  private def branches(
      entity: String,
      pos: Position,
      projectbuf: ListBuffer[(String, String)],
      joinbuf: ListBuffer[(String, String, String, String, String)],
      attrbuf: List[String]): ObjectProjectionBranch = {
    ObjectProjectionBranch(model.list(entity, pos) map {
      case (field, attr: PrimitiveEntityAttribute) =>
        val e = if (attrbuf == Nil) entity else attrbuf mkString "$"

        projectbuf += (e -> field)
        PrimitiveProjectionNode(e, field, attr)
      case (field, attr: ObjectEntityAttribute) =>
        if (attr.entity.pk isEmpty)
          problem(
            pos,
            s"entity '${attr.entityType}' is referenced as a type but has no primary key")

        val attrbuf1 = attr.entityType :: attrbuf

        joinbuf += ((entity,
                     field,
                     attr.entityType,
                     attrbuf1 mkString "$",
                     attr.entity.pk.get))
        EntityProjectionNode(
          entity,
          field,
          branches(attr.entityType, pos, projectbuf, joinbuf, attrbuf1))
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
