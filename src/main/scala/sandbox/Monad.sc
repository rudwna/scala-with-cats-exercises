import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(func andThen pure)
}

val opta = Option(1)
val optb = Option(2)
val optc = Option(3)

opta.map(a => optb.map(b => a + b))


opta.map(a => optb.map(b => optc.map(a + b + _)))