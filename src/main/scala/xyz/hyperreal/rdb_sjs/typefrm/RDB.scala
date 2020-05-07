package xyz.hyperreal.rdb_sjs.typefrm

import xyz.hyperreal.rdb_sjs.{Connection, RelationResult}

import scala.scalajs.js
import js.annotation.{JSExport, JSExportTopLevel}
import js.JSConverters._

@JSExportTopLevel("RDB")
class RDB(data: String) {

  private val conn = new Connection {
    load(data, true)
  }

  @JSExport
  def query(sql: String) = {
    conn.executeSQLStatement(sql) match {
      case RelationResult(rel) =>
        val l = rel.collect map { r =>
          js.Dictionary((rel.metadata.header zip r) map {
            case (h, f) => (h.column, f)
          }: _*)
        }

        l.toJSArray
      case _ => sys.error(s"not a relation: $sql")
    }
  }

}
