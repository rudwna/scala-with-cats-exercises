// WIP
//trait Codec[A] {
//  def encode(value: A): String
//  def decode(value: String): A
//  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
//    val self: Codec[B] = this
//    override def encode(value: B): String = self.encode(enc(value))
//    override def decode(value: B): A = dec(value)
//  }
//}

// Book's solution. Note the missing override keyword which allowed type changes.
// Codec[A].encode: A => String
// Codec[B].encode: B => String
// Codec[A].decode: String => A
// Codec[B].decode: String => B
trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self: Codec[A] = this
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))

      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

implicit val intCodec: Codec[Int] =
  stringCodec.imap(_.toInt, _.toString)

implicit val booleanCodec: Codec[Boolean] =
  stringCodec.imap(_.toBoolean, _.toString)

implicit val doubleCodec: Codec[Double] =
  stringCodec.imap(_.toDouble, _.toString)

case class Box[A](value: A)

// ❎
implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
  stringCodec.imap(Box(_), b => c.encode(b.value))

// Book's solution
// This is correct, consider that it should work with any type inside Box
// Instead of just String
implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
  c.imap[Box[A]](Box(_), _.value)