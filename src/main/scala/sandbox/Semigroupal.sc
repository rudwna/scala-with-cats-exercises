import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  x.flatMap(x => y.map(y => (x, y)))
