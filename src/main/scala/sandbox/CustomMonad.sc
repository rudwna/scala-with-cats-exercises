sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

import cats.Monad

//val treeMonad = new Monad[Tree] {
//  override def pure[A](x: A): Tree[A] = leaf(x)
//
//  override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
//      // If we don't do flatMap then it's only process single level of the tree
////    case Branch(left, right) => Branch(f(left), f(right))
//    case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
//    case Leaf(value) => f(value)
//  }
//
////  Attempt
//  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
//      // If it's leaf, then can it be Leaf(Left(*))? or it can only be Leaf(Right(*))
//    case Leaf(Left(left)) =>
//    case Leaf(Right(right)) =>
//    case Branch(left, right) => Branch(tailRecM(left)(f), tailRecM(right)(f))
//  }
//
////  Book's solution
////  Why does it use flatmap(func(a))?
////  def tailRecM[A, B](a: A)
////                    (func: A => Tree[Either[A, B]]): Tree[B] =
////    flatMap(func(a)) {
////      case Left(value) =>
////        tailRecM(value)(func)
////      case Right(value) =>
////        Leaf(value)
////    }
//}

// **Not fully understand, need to revisit**
implicit val treeMonad2 = new Monad[Tree] {
  def pure[A](value: A): Tree[A] =
    Leaf(value)

  def flatMap[A, B](tree: Tree[A])
                   (func: A => Tree[B]): Tree[B] =
    tree match {
      case Branch(l, r) =>
        Branch(flatMap(l)(func), flatMap(r)(func))
      case Leaf(value)  =>
        func(value)
    }

  def tailRecM[A, B](a: A)
                    (func: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(func(a)) {
      case Left(value) =>
        tailRecM(value)(func)
      case Right(value) =>
        Leaf(value)
    }
}

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
//  c <- branch(leaf(b - 1), leaf(b + 1))
} yield b