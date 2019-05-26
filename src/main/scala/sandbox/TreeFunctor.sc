import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

object TreeFunctor {
  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case branch: Branch[A] => Branch(map(branch.left)(f), map(branch.right)(f))
      case leaf: Leaf[A] => Leaf(f(leaf.value))
    }
  }


}

// Type hinting is required since Scala will not look for type instance of Branch
val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
val leaf = Leaf(1)
import cats.syntax.functor._
import TreeFunctor._


tree.map(_ + 1)
Functor[Tree].map(tree)(_ + 1)
Functor[Tree].map(leaf)(_ + 1)
