import cats._
import cats.syntax.all._

final case class Cat(name: String, age: Int, color: String)

implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

val garfield = Cat("Garfield", 38, "ginger and black")
println(garfield.show)