import cats.{Monad, Semigroupal}
import cats.data.Validated
import cats.syntax.flatMap._
import cats.syntax.functor._

def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  x.flatMap(x => y.map(y => (x, y)))

type AllErrorsOr[A] = Validated[String, A]
import cats.instances.string._
Semigroupal[AllErrorsOr]
