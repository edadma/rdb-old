package xyz.hyperreal.rdb_sjs

class OQL( model: String ) {


  def query( s: String ) = {
    val p = new OQLParser
    val OQLQuery(resource, select, project, order, group) = p.parseFromString(s, p.query)


  }

}