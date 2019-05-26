import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.instances.option._ // for Monoid
import cats.syntax.semigroup._ // for |+|

case class Order(totalCost: Double, quantity: Double)

object SuperAdder {
  implicit def orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty = Order(0, 0)
    override def combine(x: Order, y: Order) =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}
