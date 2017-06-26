package xyz.hyperreal.rdb


object RQLEvaluator {
	def evalRelation( ast: Any ) = {
		ast match {
			case RelationLit( columns, data ) =>
				var hset = Set[String]()

//				for (ColumnSpec( Ident(p, n), _ ) <- columns)
				for (Ident( p, n ) <- columns)
					if (hset(n))
						problem( p, s"duplicate $n" )
					else
						hset += n

//				(columns map {case ColumnSpec( Ident(_, n), _ ) => n}, data map {_ map evalExpression})
				val header = columns map {case Ident( _, n ) => n}
				val body = data map {_ map evalExpression}

				ConcreteRelation( )
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