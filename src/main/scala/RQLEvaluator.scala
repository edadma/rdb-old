package xyz.hyperreal.rdb


object RQLEvaluator {
	def evalRelation( ast: Any ) = {
		ast match {
			case ('table, headings: List[_], data: List[_]) =>
				(headings map {case ('ident, n: String) => n}, data map {case ('tuple, t: List[_]) => t map evalExpression})
		}
	}

	def evalExpression( ast: Any ): AnyRef = {
		ast match {
			case ('number, n: String) => n.toDouble.asInstanceOf[Number]
			case ('integer, n: String) => n.toInt.asInstanceOf[Number]
			case ('string, n: String) => n
		}
	}

	def evalLogical() {}
}