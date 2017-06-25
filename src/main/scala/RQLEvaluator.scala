package xyz.hyperreal.rdb


object RQLEvaluator {
	def evalRelation( ast: Any ) = {
		ast match {
			case ('table, headings: List[_], data: List[_]) =>
				var hset = Set[String]()

				for (Ident( p, n ) <- headings)
					if (hset(n))
						problem( p, s"duplicate $n" )
					else
						hset += n

				(headings map {case Ident( _, n ) => n}, data map {case ('tuple, t: List[_]) => t map evalExpression})
		}
	}

	def evalExpression( ast: Any ): AnyRef = {
		ast match {
			case NumberLit( _, n ) => n.toDouble.asInstanceOf[Number]
			case IntegerLit( _, n ) => n.toInt.asInstanceOf[Number]
			case StringLit( _, s ) => s
		}
	}

	def evalLogical() {}
}