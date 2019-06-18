import cats.Semigroup
import cats.syntax.either._    // for asLeft and asRight

final case class CheckF[E, A](f: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = f(value)
  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => s.combine(e1, e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
    }
}

import cats.instances.list._ // for Semigroup

val a: CheckF[List[String], Int] =
  CheckF { v =>
    if(v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

val b: CheckF[List[String], Int] =
  CheckF { v =>
    if(v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

val check: CheckF[List[String], Int] =
  a and b

check(5)

