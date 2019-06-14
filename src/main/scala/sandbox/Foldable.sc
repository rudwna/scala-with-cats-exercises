val list = List(1, 2, 3)

list.foldLeft(List.empty[Int])((a, i) => i :: a)
list.foldRight(List.empty[Int])((i, a) => i :: a)

def map[A, B](as: List[A])(f: A => B): List[B] =
  as.foldRight(List.empty[B])((i, a) => f(i) :: a)

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  as.foldRight(List.empty[B])((i, a) => f(i) ::: a)

def filter[A](as: List[A])(predicate: A => Boolean): List[A] =
  as.foldRight(List.empty[A])((i, a) => if (predicate(i)) i :: a else a)

def sum[A <: Int](as: List[A]): Int = as.foldRight(0)(_ + _)

map(list)(_ + 1)
flatMap(list)(x => List(x * 1, x * 2, x * 3))
filter(list)(_ > 1)
sum(list)