import cats.Monoid

import scala.concurrent.Future
import cats.syntax.semigroup._ // for |+|

def foldMap[A, B : Monoid](as: Vector[A])(f: A => B): B =
  as.map(f).foldLeft(implicitly[Monoid[B]].empty)(implicitly[Monoid[B]].combine)
//as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)
//as.foldLeft(Monoid[B].empty)(_ |+| func(_))

import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.future._

def parallelFoldMap[A, B : Monoid](as: Vector[A])(f: A => B): Future[B] =
//val groupSize = (1.0 * values.size / numCores).ceil.toInt
as.grouped(Runtime.getRuntime.availableProcessors())
  .map(batch => Future(foldMap(batch)(f)))
  .toList
  .sequence
  .map(_.foldLeft(Monoid[B].empty)(_ |+| _))

// foldMap need to be more abstract for below line to work
//  foldMap(as.grouped(Runtime.getRuntime.availableProcessors()))(batch => Future(foldMap(batch)(f)))

import cats.syntax.foldable._
import cats.instances.vector._

def catsParallelFoldMap[A, B : Monoid](as: Vector[A])(f: A => B): Future[B] =
//val groupSize = (1.0 * values.size / numCores).ceil.toInt
  as.grouped(Runtime.getRuntime.availableProcessors())
    .map(batch => Future(batch.foldMap(f)))
    .toList
    .sequence
    .map(_.foldLeft(Monoid[B].empty)(_ |+| _))
