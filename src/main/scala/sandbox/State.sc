import cats.data.State

val step1 = State[Int, String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
} yield (a, b)

val (state, result) = both.run(20).value

/////////////////////// Exercise below

import cats.data.State

type CalcState[A] = State[List[Int], A]

def operator(f: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case b :: a :: tail =>
      val result = f(a, b)
      (result :: tail, result)
  }

def evalOne(sym: String): CalcState[Int] = sym match {
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case i => State[List[Int], Int] { oldState =>
    (i.toInt :: oldState, i.toInt)
  }
}


evalOne("42").runA(Nil).value

val program = for {
  _   <- evalOne("1")
  _   <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program.runA(Nil).value

import cats.syntax.applicative._

def evalAll(input: List[String]): CalcState[Int] =
  input.map(evalOne).foldLeft(State.pure[List[Int], Int](0))((s, b) => s.flatMap(_ => b))

// Book's solution
def evalAllSolution(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

val program2 = evalAll(List("1", "2", "+", "3", "*"))
program2.runA(Nil).value

val program3 = for {
  _   <- evalAll(List("1", "2", "+"))
  _   <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans
program3.runA(Nil).value

def evalInput(expr: String, init: List[Int]): Int = evalAll(expr.split(' ').toList).runA(init).value