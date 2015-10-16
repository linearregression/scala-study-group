trait Stream[+A] {
  import Stream._
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  // Exercise 1
  def toList: List[A] = uncons match {
    case None => Nil
    case Some((head, tail)) => head :: tail.toList
  }
  // Exercise 2
  def take(n: Int): Stream[A] = uncons match {
    case Some((head, tail)) if n > 0 => cons(head, tail.take(n - 1))
    case _ => empty
  }
  // Exercise 3
  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((head, tail)) if p(head) => cons(head, tail.takeWhile(p))
    case _ => empty
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  // Exercise 4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)
  // Exercise 5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, tail) => if (p(a)) cons(a, tail) else empty)
  // Exercise 6
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, tail) => cons(f(a), tail))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, tail) => if (p(a)) cons(a, tail) else tail)
  def append[T >: A](that: => Stream[T]): Stream[T] =
    foldRight(that)((a, acc) => cons(a, acc))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, tail) => f(a).append(tail))
  def mapViaUnfold [B](f: A => B): Stream[B] =
    unfold(this)(s => s.uncons match {
      case None => None
      case Some((h, t)) => Some((f(h), t))
    })

}
object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  val ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def loop(a1:Int, a2:Int): Stream[Int] = {
      cons(a1, loop(a2, a1+a2))
    }
    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f)) 
  }
  def fibsViaUnfold: Stream[Int] =
    unfold((0,1))(a => Some((a._1,(a._2, a._1 + a._2))))
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(a => Some((a, a + 1)))
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))
  def onesViaConstantViaUnfold: Stream[Int] = 
    constantViaUnfold(1)
}
List(1, 2, 3) == Stream(1, 2, 3).toList
List(List(1,2), List(3)) == Stream(List(1,2), List(3)).toList
Nil == Stream.empty.toList
List(1,2) == Stream(1,2,3).take(2).toList
List(1,2,3) == Stream(1,2,3).take(5).toList
Nil == Stream(1,2,3).take(-1).toList
List(1) == Stream(1).take(1).toList
val emptyStreamOfInts = Stream.empty[Int]
emptyStreamOfInts.toList
Stream.apply(1, Stream.empty).toList
Stream.cons(1, Stream.empty).toList
List(1,2,3) == Stream(1,2,3,4,5).takeWhile(_ < 4).toList
Nil == Stream(1,2,3,4,5).takeWhile(_ > 4).toList
Stream(2,4,6).forAll(_ % 2 == 0)
!Stream(2,4,6).forAll(_ % 3 == 0)
!Stream(1,3,6).forAll(_ % 3 == 0)
List(1,2,3) == Stream(1,2,3,4,5).takeWhileViaFoldRight(_ < 4).toList
Nil == Stream(1,2,3,4,5).takeWhileViaFoldRight(_ > 4).toList
List("1", "2", "3") == Stream(1,2,3).map(_.toString).toList
List(1,3) == Stream(1,2,3).filter(_ % 2 != 0).toList
List(1,2,3,4) == Stream(1,2).append(Stream(3,4)).toList
List(1,1,2,2) == Stream(1,2).flatMap(a => Stream(a, a)).toList
List() == Stream(1,2).flatMap(a => Stream()).toList
// StackOverflowError
// List(2,2,2,2,2) == Stream.ones.map(_ + 1).filter(_ == 2).toList
// List() == Stream.ones.filter(_ == 2).take(1).toList
// true == Stream.ones.forAll(_ == 1)
List(2,2,2,2,2) == Stream.ones.map(_ + 1).take(5).toList
List(2,2,2,2,2) == Stream.constant(1).map(_ + 1).take(5).toList
List(2,3) == Stream.from(1).map(_ + 1).take(2).toList
List(0, 1, 1, 2, 3, 5, 8) == Stream.fibs.take(7).toList
List(0, 1, 1, 2, 3) == Stream.unfold((0,1))(a => Some((a._1,(a._2, a._1 + a._2)))).take(5).toList
List(0, 1, 1, 2, 3) == Stream.fibsViaUnfold.take(5).toList
List(2,3) == Stream.fromViaUnfold(1).map(_ + 1).take(2).toList
List(2,2,2,2,2) == Stream.constantViaUnfold(1).map(_ + 1).take(5).toList
List(2,2,2,2,2) == Stream.onesViaConstantViaUnfold.map(_ + 1).take(5).toList
List("1", "2", "3") == Stream(1,2,3).mapViaUnfold(_.toString).toList
