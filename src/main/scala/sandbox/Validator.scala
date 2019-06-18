package sandbox

import cats.syntax.validated._    // for asLeft and asRight
import cats.instances.list._
import cats.Semigroup
import cats.data.Validated._
import cats.data.Validated
import cats.syntax.apply._     // for mapN
import cats.syntax.semigroup._

object Validator extends App {
  final case class And[E, A](
                              left: Check[E, A],
                              right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](
                               func: A => Validated[E, A]) extends Check[E, A]
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          // Validated((x, y)).mapN
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a)    => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  val a: Check[List[String], Int] =
    Pure { v =>
      if(v > 2) v.valid
      else List("Must be > 2").invalid
    }

  val b: Check[List[String], Int] =
    Pure { v =>
      if(v < -2) v.valid
      else List("Must be < -2").invalid
    }

  val check: Check[List[String], Int] =
    a and b

  println(check(0))
}
