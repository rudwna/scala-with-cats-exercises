import cats.data.Reader
import cats.syntax.applicative._

case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(
                   username: String,
                   password: String): DbReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))

// Note that the solution wrap Option-related stuff inside
// And only work with Reader monad on for-comp level
def checkLogin(
                userId: Int,
                password: String): DbReader[Boolean] =
  for {
    username <- findUsername(userId)
    passwordOk <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
  } yield passwordOk
