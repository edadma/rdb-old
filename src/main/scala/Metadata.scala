package xyz.hyperreal.rdb

import scala.util.parsing.input.Position


class Metadata( val header: IndexedSeq[Column] ) {

	lazy val tableSet = header map (_.table) toSet

	lazy val columnMap = (header map (_.column) zipWithIndex) toMap

	lazy val tableColumnMap = (header map {case Column(t, c, _, _) => (t, c)} zipWithIndex) toMap

	lazy val attributes = header map {case Column(_, n, t, _) => (n, t)} toSet

}