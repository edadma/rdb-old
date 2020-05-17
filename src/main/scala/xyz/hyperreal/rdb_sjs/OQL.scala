package xyz.hyperreal.rdb_sjs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.JSON

object OQL {

  def toJS(a: Any): js.Any =
    a match {
      case l: Seq[_] => l map toJS toJSArray
      case m: Map[_, _] =>
        (m map { case (k, v) => k -> toJS(v) })
          .asInstanceOf[Map[String, Any]]
          .toJSDictionary
      case _ => a.asInstanceOf[js.Any]
    }

  def pretty(res: Seq[Map[String, Any]]): String =
    JSON.stringify(toJS(res), null.asInstanceOf[js.Array[js.Any]], 2)

}

class OQL(erd: String) {

  private val model = new ERModel(erd)

  def query(s: String, conn: Connection): Seq[Map[String, Any]] = {
    val OQLQuery(resource, project, select, order, restrict) =
      OQLParser.parseQuery(s)
    val entity = model.get(resource.name, resource.pos)
    val joinbuf = new ListBuffer[(String, String, String, String, String)]
    val graph =
      branches(resource.name, entity, project, joinbuf, List(resource.name))

    val sql = new StringBuilder
    val wherebuf = new StringBuilder

    sql append s"SELECT *\n"
    sql append s"  FROM ${resource.name}"

    def where(expr: ExpressionOQL): Unit =
      expr match {
        case InfixExpressionOQL(left, op, right) =>
          where(left)
          wherebuf append s" $op "
          where(right)
        case PrefixExpressionOQL(op, expr) =>
          wherebuf append s" $op"
          where(expr)
        case FloatLiteralOQL(n)         => wherebuf append n
        case IntegerLiteralOQL(n)       => wherebuf append n
        case StringLiteralOQL(s)        => wherebuf append s"'$s'"
        case GroupedExpressionOQL(expr) => wherebuf append s"($expr)"
        case VariableExpressionOQL(ids) =>
          wherebuf append reference(resource.name, entity, ids, joinbuf)
//          val ns = ids map (_.name)
//          val entity = (resource.name :: ns.init).reverse mkString "$"
//          val vselect = s"$entity.${ns.last}"
//
//          if (!entityset(entity)) {
//            problem(
//              v.pos,
//              s"object not found: ${(resource.name :: ns.init) mkString "."}")
//          }
//
//          sql append vselect
      }

    if (select isDefined)
      where(select.get)

    for ((lt, lf, rt, rta, rf) <- joinbuf.toSet)
      sql append s" JOIN $rt AS $rta ON $lt.$lf = $rta.$rf"

    sql append '\n'

    if (select isDefined) {
      sql append "  WHERE "
      sql append wherebuf
      sql append '\n'
    }

    print(sql)

    val res =
      conn
        .executeSQLStatement(sql.toString)
        .asInstanceOf[RelationResult]
        .relation
        .collect

    res.toList map (build(_, res.metadata, graph))
  }

  private def reference(
      entityname: String,
      entity: Entity,
      ids: List[Ident],
      joinbuf: ListBuffer[(String, String, String, String, String)],
  ) = {
    def reference(entityname: String,
                  entity: Entity,
                  ids: List[Ident],
                  attrlist: List[String]): String =
      ids match {
        case attr :: tail =>
          entity.attributes get attr.name match {
            case None =>
              problem(attr.pos,
                      s"$entityname doesn't have attribute '${attr.name}'")
            case Some(_: PrimitiveEntityAttribute) =>
              s"${attrlist mkString "$"}.${attr.name}"
            case Some(ObjectEntityAttribute(entityType, entity)) =>
              if (tail == Nil)
                problem(attr.pos,
                        s"attribute '${attr.name}' has non-primitive data type")
              else {
                val attrlist1 = attr.name :: attrlist

                joinbuf += ((attrlist mkString "$",
                             attr.name,
                             entityType,
                             attrlist1 mkString "$",
                             entity.pk.get))
                reference(entityType, entity, tail, attrlist1)
              }
          }
      }

    reference(entityname, entity, ids, List(entityname))
  }

  private def branches(
      entityname: String,
      entity: Entity,
      project: ProjectExpressionOQL,
      joinbuf: ListBuffer[(String, String, String, String, String)],
      attrlist: List[String]): ObjectProjectionBranch = {
    val attrs =
      if (project == ProjectAllOQL) {
        entity.attributes map { case (k, v) => (k, v, ProjectAllOQL) } toList
      } else
        project.asInstanceOf[ProjectAttributesOQL].attrs map (attr =>
          entity.attributes get attr.attr.name match {
            case None =>
              problem(attr.attr.pos, s"unknown attribute: '${attr.attr.name}'")
            case Some(typ) => (attr.attr.name, typ, attr.project)
          })
    ObjectProjectionBranch(attrs map {
      case (field, attr: PrimitiveEntityAttribute, _) =>
        PrimitiveProjectionNode(attrlist mkString "$", field, attr)
      case (field, attr: ObjectEntityAttribute, project) =>
        if (attr.entity.pk isEmpty)
          problem(
            null,
            s"entity '${attr.entityType}' is referenced as a type but has no primary key") // todo: entities that are being referenced should have a primary key

        val attrlist1 = field :: attrlist

        joinbuf += ((attrlist mkString "$",
                     field,
                     attr.entityType,
                     attrlist1 mkString "$",
                     attr.entity.pk.get))
        EntityProjectionNode(
          entityname,
          field,
          branches(attr.entityType, attr.entity, project, joinbuf, attrlist1)
        )
    })
  }

  private def build(row: Tuple, md: Metadata, branch: ProjectionBranch) = {
    def build(branch: ProjectionBranch): Map[String, Any] =
      branch match {
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
