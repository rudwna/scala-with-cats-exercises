import cats.instances.vector._
import cats.syntax.applicative._
import cats.data.Writer
import cats.syntax.writer._
import cats.instances.int._
import cats.syntax.show._ // for show
import cats.instances.list._

type Logged[A] = Writer[Vector[String], A]

//object x {
//  for {
//    a <- Option(1)
//    _ <- Option(0)
//    b <- Option(1)
//  } yield a
//}
// Is desugared into:
//  Option.apply(1)
//    .flatMap((a: Int) =>
//      Option.apply(0)
//        .flatMap((_: Int) =>
//          Option.apply(1)
//            .map((b: Int) => a)
//        )
//    )
// So the type of `_ <- x` statement still be Writer[]
// and the logs got carried on

// Here a and b represent the val that got passed
// as param to flatMap(f: A => F[B])
//                        ^ this value is named a and b
for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

def logNumber(x: Int): Writer[List[String], Int] =
  Writer(List("Got number: " + x.show), 3)
def multWithLog: Writer[List[String], Int] =
  for {
    a <- logNumber(3)
    b <- logNumber(5)
  } yield a * b
multWithLog.run