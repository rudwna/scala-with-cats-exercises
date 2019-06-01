//def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
//  as match {
//    case head :: tail =>
//      fn(head, foldRight(tail, acc)(fn))
//    case Nil =>
//      acc
//  }

import cats.Eval

// [x]
//def safeFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
//  as match {
//    case head :: tail =>
//      Eval.now(fn(head, safeFoldRight(tail, acc)(fn)))
//    case Nil =>
//      Eval.now(acc)
//  }

// Book's solution
def foldRightEval[A, B](as: List[A], acc: Eval[B])
                       (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc
  }

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc)) { (a, b) =>
    b.map(fn(a, _))
  }.value

foldRight((1 to 100000).toList, 0L)(_ + _)
