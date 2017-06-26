package xyz.hyperreal.rdb


object RQLEvaluator {
	def evalRelation( ast: Any ) = {
		ast match {
			case RelationLit( columns, data ) =>
				var hset = Set[String]()

				for (ColumnSpec( Ident(p, n), _ ) <- columns)
					if (hset(n))
						problem( p, s"duplicate $n" )
					else
						hset += n

				(columns map {case ColumnSpec( Ident(_, n), _ ) => n}, data map {_ map evalExpression})
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