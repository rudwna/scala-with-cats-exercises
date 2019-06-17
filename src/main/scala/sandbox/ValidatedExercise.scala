package sandbox

// Seems to get error in worksheet but works fine here
object ValidatedExercise extends App {
  case class User(name: String, age: Int)

  import cats.data.Validated
  import cats.syntax.either._

  type FormEither[A] = Either[List[String], A]
  type FormValidated[A] = Validated[List[String], A]

  def getValue(key: String, data: Map[String, String]): FormEither[String] =
    data.get(key).toRight(List(s"Key: $key not found."))

  val data = Map("x" -> "y")

  getValue("z", data)

  def parseInt(value: String): FormEither[Int] =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => List(s"Cannot parse: $value into integer"))

  parseInt("1")

  //def nonBlank(value: String): FormValidated[String] =
  //  Validated.cond(value.isEmpty, value, List("Value cannot be empty"))
  //
  //def nonNegative(value: Int): FormValidated[Int] =
  //  Validated.cond(value < 0, value, List(s"Value cannot be non negative(got $value)"))

  def nonBlank(value: String): FormEither[String] =
    //  Either.cond(value.isEmpty, value, List("Value cannot be empty"))
    Right(value).ensure(List("Value cannot be empty"))(!_.isEmpty)

  def nonNegative(value: Int): FormEither[Int] =
    //  Either.cond(value < 0, value, List(s"Value cannot be non negative(got $value)"))
    Right(value).ensure(List(s"Value cannot be non negative(got $value)"))(
      _ >= 0)

  def readName(key: String, data: Map[String, String]): FormEither[String] =
    for {
      name <- getValue(key, data)
      nonBlankName <- nonBlank(name)
    } yield nonBlankName

  def readAge(key: String, data: Map[String, String]): FormEither[Int] =
    for {
      ageString <- getValue(key, data)
      age <- parseInt(ageString)
      validAge <- nonNegative(age)
    } yield validAge

  val formData = Map("name" -> "RW", "age" -> "8")

  import cats.syntax.apply._
  import cats.syntax.either._
  import cats.instances.list._

  println(
    (readName("name", formData).toValidated,
     readAge("age", formData).toValidated).mapN(User.apply))
}
