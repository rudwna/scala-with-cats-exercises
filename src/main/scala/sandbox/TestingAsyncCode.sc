import cats.{Applicative, Id}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.Future

trait UptimeClient[F[_]] {
    def getUptime(host: String): F[Int]
}

trait RealUptimeClient[A] extends UptimeClient[Future] {
    def getUptime(host: String): Future[Int]
}

trait TestUptimeClient[A] extends UptimeClient[Id] {
    def getUptime(host: String): Id[Int]
}

class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
}