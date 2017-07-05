package xyz.hyperreal

import scala.util.parsing.input.Position


package object rdb {
	val VERSION = "0.1"

	type Tuple = Vector[AnyRef]

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else
			sys.error( pos.line + ": " + error + "\n" + pos.longString )

	object TRUE extends Logical {
		def not = FALSE

		def and( that: Logical ) =
			that match {
				case TRUE => TRUE
				case FALSE => FALSE
				case MAYBE_A => MAYBE_A
				case MAYBE_I => MAYBE_I
			}

		def or( that: Logical ) = TRUE
	}

	object FALSE extends Logical {
		def not = TRUE

		def and( that: Logical ) = FALSE

		def or( that: Logical ) =
			that match {
				case TRUE => TRUE
				case FALSE => FALSE
				case MAYBE_A => MAYBE_A
				case MAYBE_I => FALSE
			}
	}

	object MAYBE_A extends Logical {
		def not = MAYBE_A

		def and( that: Logical ) =
			that match {
				case TRUE | MAYBE_A => MAYBE_A
				case FALSE => FALSE
				case MAYBE_I => MAYBE_I
			}

		def or( that: Logical ) =
			that match {
				case TRUE => TRUE
				case FALSE | MAYBE_A | MAYBE_I => MAYBE_A
			}
	}

	object MAYBE_I extends Logical {
		def not = MAYBE_I

		def and( that: Logical ) =
			that match {
				case TRUE | MAYBE_A | MAYBE_I => MAYBE_I
				case FALSE => FALSE
			}

		def or( that: Logical ) =
			that match {
				case TRUE => TRUE
				case FALSE => FALSE
				case MAYBE_A => MAYBE_A
				case MAYBE_I => MAYBE_I
			}
	}

	object A extends Mark( "a-marked" ) {
		def scala( that: AnyRef ) = if (that == I) I else A

		def comparison( that: AnyRef ) = if (that == I) MAYBE_I else MAYBE_A
	}

	object I extends Mark( "i-marked" ) {
		def scala( that: AnyRef ) = I

		def comparison( that: AnyRef ) = MAYBE_I
	}
}