import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
}
def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(0.0, _) => 0.0
  case Cons(x, xs) => x * product(xs)
}
val xx = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}
def tail[A](as: List[A]): List[A] = as match {
  case Cons(_, t) => t
  case Nil => Nil
}
List(2, 3) == tail(List(1, 2, 3))
Nil == tail(List(1))
Nil == tail(Nil)
Nil == tail(List(Nil))
def drop[A](n: Int, as: List[A]): List[A] = as match {
  case Nil => Nil
  case _ if n <= 0 => as
  case Cons(_, t) => drop(n - 1, t)
}
List(3, 4) == drop(2, List(1, 2, 3, 4))
Nil == drop(2, List(1, 2))
Nil == drop(2, List(1))
List(1, 2) == drop(0, List(1, 2))
List(1, 2) == drop(-3, List(1, 2))
@tailrec
def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Nil => Nil
  case Cons(head, tail) if f(head) => dropWhile(tail)(f)
  case _ => as
}
List(4, 5) == dropWhile(List(1, 3, 4, 5))((a) => a % 2 != 0)
def setHead[A](as: List[A], a: A): List[A] = as match {
  case Cons(head, tail) => Cons(a, tail)
  case Nil => Nil
}
List(3, 5) == setHead(List(4, 5), 3)
Nil == setHead(Nil, 1)
setHead(List(List(), Nil), Nil)
setHead(List(4, 5), 3)
def init[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
List(1, 2, 3) == init(List(1, 2, 3, 4))
def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
  case Nil => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}
def sum2(l: List[Int]) =
  foldRight(l, 0.0)(_ + _)
def product2(l: List[Double]) =
  foldRight(l, 1.0)(_ * _)
def lengthRec[A](l: List[A]): Int = l match {
  case Nil => 0
  case Cons(_, t) => 1 + lengthRec(t)
}
3 == lengthRec(List(1, 2, 3))
0 == lengthRec(Nil)
0 == lengthRec(List())
def length[A](as: List[A]): Int = {
  foldRight(as, 0)((_, acc) => acc + 1)
}
3 == length(List(1, 2, 3))
0 == length(Nil)
0 == length(List())
@tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  case Nil => z
  case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
}
foldLeft(List(1, 2, 3), 0)(_ + _) == 6
def sum3(l: List[Int]) =
  foldLeft(l, 0.0)(_ + _)
def product3(l: List[Double]) =
  foldLeft(l, 1.0)(_ * _)
6 == product3(List(1, 2, 3))
6 == sum3(List(1, 2, 3))
def reverse[A](l: List[A]): List[A] =
  foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
List(3, 2, 1) == reverse(List(1, 2, 3))
def append[A](a1: List[A], a2: List[A]): List[A] =
  foldRight(a1, a2)(Cons(_, _))
List(1, 2) == append(List(1), List(2))
def lastElement[A](l: List[A]): A = l match {
  case Nil => sys.error("nein")
  case Cons(h, Nil) => h
  case Cons(_, t) => lastElement(t)
}
3 == lastElement(List(1, 2, 3))

//def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
//  foldRight(l, (Nil, z))((a, b) => )
//}
def append2[A](a1: List[A], a2: List[A]): List[A] = {
  val r1 = reverse(a1)
  reverse(foldLeft(a2, r1)((b, a) => Cons(a, b)))
}
List(1, 2, 3, 4) == append2(List(1, 2), List(3, 4))
def flatten[A](ll: List[List[A]]): List[A] = {
  reverse(foldLeft(reverse(ll), Nil: List[A])((b, a) => append(b, reverse(a))))
}
List(1, 2, 3, 4) == flatten(List(List(1, 2), List(3, 4)))

// def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B
// def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B
def foldLeftViaFoldRight[X, Y](xs: List[X], foldLeftInitial: Y)(foldLeftFn: (Y, X) => Y): Y = {

  def transformerFn(x: X, g: (Y) => Y): ((Y) => Y) =
    foldLeftAcc => g(foldLeftFn(foldLeftAcc, x))

  foldRight(xs, (identity[Y] _))(transformerFn)(foldLeftInitial)
}
def flatten2[A](ll: List[List[A]]): List[A] = {
  val rl = reverse(ll)
  reverse(foldLeft(rl, Nil: List[A])((b, a) => append(b, reverse(a))))
}
flatten2(List(List(1, 2), List(3, 4)))
def incrAll(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case Cons(h, t) => Cons(h + 1, incrAll(t))
  //  case h :: t => h + 1 :: incrAll(t)
}
List(2, 3, 4) == incrAll(List(1, 2, 3))
def toDoubleAll(xs: List[Double]): List[String] = xs match {
  case Nil => Nil
  case Cons(h, t) => Cons(h.toString, toDoubleAll(t))
}
List("1.0", "2.0") == toDoubleAll(List(1.0, 2.0))
def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
  case Nil => Nil
  case Cons(h, t) => Cons(f(h), map(t)(f))
}
List(2, 3) == map(List(1, 2))(1 + _)
def filter[A](xs: List[A])(f: (A) => Boolean): List[A] = xs match {
  case Nil => Nil
  case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
  case Cons(_, t) => filter(t)(f)
}
List(2, 4) == filter(List(1, 2, 3, 4))(_ % 2 == 0)
def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
  //  foldLeft(xs, Nil: List[B])((b: List[B],))
  foldRight(xs, Nil: List[B])((x: A, bs: List[B]) => append(f(x), bs))
}
List(1, 1, 2, 2) == flatMap(List(1, 2))(i => List(i, i))
def filterViaFlatMap[A](xs: List[A])(f: (A) => Boolean): List[A] = {
  flatMap(xs)(i => if (f(i)) List(i) else Nil)
}
List(2, 4) == filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0)
def addLists(xs1: List[Int], xs2: List[Int]): List[Int] = (xs1, xs2) match {
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  case _ => Nil
}

List(5, 7, 9) == addLists(List(1, 2, 3, 4), List(4, 5, 6))
List(5, 7, 9) == addLists(List(1, 2, 3), List(4, 5, 6, 8))
Nil == addLists(Nil, Nil)
Nil == addLists(List(1,2), Nil)

def zipWith[X, Y, Z](xs: List[X], ys: List[Y])(f: (X, Y) => Z): List[Z] = (xs, ys) match {
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  case _ => Nil
}
List(5, 7, 9) == zipWith(List(1, 2, 3, 4), List(4, 5, 6))(_ + _)
