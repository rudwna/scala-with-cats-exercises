trait Printable[A] {
  def format(value: A): String
}

final case class Cat(name: String, age: Int, color: String)

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(p.format(value))
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (value: Int) => value.toString

  implicit val printableString: Printable[String] = (value: String) => value

  implicit val printableCat: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat."
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](printable: A) {
    def format(implicit p: Printable[A]): String = p.format(printable)
    def print(implicit p: Printable[A]): Unit = println(format(p))
  }
}

import PrintableSyntax._
import PrintableInstances._
val cat = Cat("Garfield", 38, "ginger and black")
cat.print