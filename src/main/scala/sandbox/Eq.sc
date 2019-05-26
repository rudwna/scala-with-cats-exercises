import cats.Eq
import cats.syntax.eq._

final case class Cat(name: String, age: Int, color: String)

implicit val catEq: Eq[Cat] = Eq.instance { (a, b) =>
  a.name == b.name && a.color == b.color && a.age == b.age
}

val cat1 = Cat("Garfield",   38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option(cat2)

cat1 === cat2