import cats.{Monoid, Semigroup}
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._ // for |+|

object BooleanMonoid {
  implicit val booleanAndMonoid: Monoid[Boolean] = {
    def combine(x: Boolean, y: Boolean): Boolean = x && y
    def empty: Boolean = true
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = {
    def combine(x: Boolean, y: Boolean): Boolean = x || y
    def empty: Boolean = false
  }

  implicit val booleanXorMonoid: Monoid[Boolean] = {
    def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
    def empty: Boolean = false
  }

  implicit val booleanXnorMonoid: Monoid[Boolean] = {
    def combine(x: Boolean, y: Boolean): Boolean = x == y
    def empty: Boolean = true
  }
}

object SetMonoid {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    def empty: Set[A] = Set.empty
  }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x.intersect(y)

  implicit def setDifferenceMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x -- y
    def empty: Set[A] = Set.empty
  }
}