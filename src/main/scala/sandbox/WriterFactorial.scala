//package sandbox
//import cats.data.Writer
//import cats.syntax.applicative._
//import cats.syntax.writer._
//import cats.instances.vector._
//
//object WriterFactorial extends App {
//  type Logged[A] = Writer[Vector[String], A]
//
//  def slowly[A](body: => A) =
//    try body
//    finally Thread.sleep(100)
//
//  def factorial(n: Int): Int = {
//    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
//    println(s"fact $n $ans")
//    ans
//  }
//
//  def factorialW(
//      nw: Logged[Int]): Logged[Int] = {
////    val ans = slowly(if (nw.value == 0) 1 else nw.flatMap(n => factorialW((n - 1).pure[Logged]).map(m => n * m)))
//
//    val ans = if (nw.value == 0) 1.pure[Logged]
//    else
//      for {
//        n <- nw
//        _ <- factorialW(nw.map(_ - 1))
//      } yield n.writer(Vector(s"fact $n $ans"))
//
//    ans
//  }
//
//  import scala.concurrent._
//  import scala.concurrent.ExecutionContext.Implicits.global
//  import scala.concurrent.duration._
//
//  val a = Await.result(Future.sequence(
//                 Vector(
//                   Future(factorialW(3.pure[Logged])),
//                   Future(factorialW(3.pure[Logged]))
//                 )),
//               5.seconds)
//
//  println(a)
//}
// Above is unsuccessful attempts