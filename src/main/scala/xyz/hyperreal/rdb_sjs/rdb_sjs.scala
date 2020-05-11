package xyz.hyperreal

import scala.util.parsing.input.Position

package object rdb_sjs {
  val VERSION = "0.1"

  type Tuple = IndexedSeq[Any]

  def problem(pos: Position, error: String) =
    if (pos eq null)
      sys.error(error)
    else if (pos.line == 1)
      sys.error(s"$error\n${pos.longString}")
    else
      sys.error(s"${pos.line}: $error\n${pos.longString}")

  var anoncount = 1

  def anonymous = {
    val res = anoncount

    anoncount += 1
    s"_$res"
  }

  def search[E, R](list: List[E])(test: E => Option[R]) = {
    def _search(l: List[E], depth: Int): Option[(R, Int)] =
      if (l == Nil)
        None
      else
        test(l.head) match {
          case None    => _search(l.tail, depth + 1)
          case Some(r) => Some((r, depth))
        }

    _search(list, 0)
  }

  object TRUE extends Logical {
    def not = FALSE

    def and(that: => Logical) =
      that match {
        case TRUE    => TRUE
        case FALSE   => FALSE
        case MAYBE_A => MAYBE_A
        case MAYBE_I => MAYBE_I
      }

    def or(that: => Logical) = TRUE

    override def toString = "true"
  }

  object FALSE extends Logical {
    def not = TRUE

    def and(that: => Logical) = FALSE

    def or(that: => Logical) =
      that match {
        case TRUE    => TRUE
        case FALSE   => FALSE
        case MAYBE_A => MAYBE_A
        case MAYBE_I => FALSE
      }

    override def toString = "false"
  }

  object MAYBE_A extends Logical {
    def not = MAYBE_A

    def and(that: => Logical) =
      that match {
        case TRUE | MAYBE_A => MAYBE_A
        case FALSE          => FALSE
        case MAYBE_I        => MAYBE_I
      }

    def or(that: => Logical) =
      that match {
        case TRUE                      => TRUE
        case FALSE | MAYBE_A | MAYBE_I => MAYBE_A
      }

    override def toString = "maybe_a"
  }

  object MAYBE_I extends Logical {
    def not = MAYBE_I

    def and(that: => Logical) =
      that match {
        case TRUE | MAYBE_A | MAYBE_I => MAYBE_I
        case FALSE                    => FALSE
      }

    def or(that: => Logical) =
      that match {
        case TRUE    => TRUE
        case FALSE   => FALSE
        case MAYBE_A => MAYBE_A
        case MAYBE_I => MAYBE_I
      }

    override def toString = "maybe_i"
  }

  object A extends Mark("a-marked") {
    def scala(that: Any) = if (that == I) I else A

    def comparison(that: Any) = if (that == I) MAYBE_I else MAYBE_A
  }

  object I extends Mark("i-marked") {
    def scala(that: Any) = I

    def comparison(that: Any) = MAYBE_I
  }
}
