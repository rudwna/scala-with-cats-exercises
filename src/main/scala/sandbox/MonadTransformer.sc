import cats.MonadError
import cats.data.EitherT

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Future}

//type Response[A] = Future[Either[String, A]]

type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

def getPowerLevel(name: String): Response[Int] =
  powerLevels.get(name) match {
    case Some(powerLevel) => EitherT.right(Future(powerLevel))
    case None => EitherT.left(Future(s"$name is unreachable"))
  }

getPowerLevel("sdas")

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    a1 <- getPowerLevel(ally1)
    a2 <- getPowerLevel(ally2)
  } yield (a1 + a2) > 15

def tacticalReport(ally1: String, ally2: String): String = {
  val inte = canSpecialMove(ally1, ally2).value
  Await.result(inte, FiniteDuration.apply(1, "seconds")) match {
    case Left(msg) => s"Comms error: $msg"
    case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) => s"$ally1 and $ally2 need a recharge."
  }
}

// This make me confuse a bit
// So after you stack monads then if you want to manipulate
// within each monad semantic (such as transform left/right here)
// then you need to do .value?

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")
