trait Printable[A] {
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    (value: B) => format(func(value))
}

def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

implicit val stringPrintable: Printable[String] =
  (value: String) => "\"" + value + "\""

implicit val booleanPrintable: Printable[Boolean] =
  (value: Boolean) => if (value) "yes" else "no"

format("hello")
format(true)

final case class Box[A](value: A)

implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
  (box: Box[A]) => p.contramap[Box[A]](_.value).format(box)

// Book's solution, no need to create instance of Printable
// since we already have instance from implicit parameter
//implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
//  p.contramap[Box[A]](_.value)

format(Box("hello world"))
format(Box(true))