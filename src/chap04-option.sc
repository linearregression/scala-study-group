import java.util.regex.{Pattern, PatternSyntaxException}

import scala.collection.{GenTraversableOnce, GenTraversable}

// Exercise 1
trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}
case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}
case class Some[+A](v: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(v))
  def flatMap[B](f: A => Option[B]): Option[B] = f(v)
  def getOrElse[B >: A](default: => B): B = v
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if (f(v)) Some(v) else None
}
def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.size)
// Exercise 2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m =>
    mean(xs.map(x => math.pow(x - m, 2))))
None == variance(List())
2.0/3 == variance(List(1, 2, 3)).getOrElse(0)
// Exercise 2 extra
def variance2(xs: Seq[Double]): Option[Double] =
  for {
    m <- mean(xs)
    ds = for (x <- xs) yield math.pow(x - m, 2)
    v <- mean(ds)
  } yield v
None == variance2(List())
2.0/3 == variance2(List(1,2,3)).getOrElse(0)
// Exercise 3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }
}
Some(1) == map2(Some(1), Some(0))(_ + _)
None == map2(Some(1), None: Option[Int])(_ * _)
// Exercise 3 extra
def map2for[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)
}
Some(1) == map2for(Some(1), Some(0))(_ + _)
None == map2for(Some(1), None: Option[Int])(_ * _)
// Exercise 3 more extra
def map2flatmap[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a.flatMap(aa =>
    b.map(bb => f(aa, bb)))
}
Some(1) == map2flatmap(Some(1), Some(0))(_ + _)
None == map2flatmap(Some(1), None: Option[Int])(_ * _)
// Exercise 4
def pattern(s: String): Option[Pattern] =
  try {
    Some(Pattern.compile(s))
  } catch {
    case e: PatternSyntaxException => None
  }
def mkMatcher(pat: String): Option[String => Boolean] =
  pattern(pat).map(p =>
    (s: String) => p.matcher(s).matches)
def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
  map2(mkMatcher(pat1), mkMatcher(pat2))((f1, f2) => f1(s) && f2(s))
Some(true) == bothMatch_2(".*", ".+", "blabla")
Some(false) == bothMatch_2("[0-9]", ".+", "blabla")
None == bothMatch_2("[0-9]", "+", "blabla")
// Exercise 5
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case None :: _ => None
  case Some(h) :: t => sequence(t).map(tt => h :: tt)
  case _ => Some(Nil)
}
Some(List(1,2,3)) == sequence(List(Some(1), Some(2), Some(3)))
None == sequence(List(Some(1), None, Some(3)))
// Exercise 6
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h :: t => f(h).flatMap(fres => traverse(t)(f).map(tres => fres :: tres))
}
def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(identity)
Some(List(1,2,3)) == sequenceViaTraverse(List(Some(1), Some(2), Some(3)))
None == sequenceViaTraverse(List(Some(1), None, Some(3)))
// Exercise 6 extra
def traverseViaFor[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h :: t => for {
    fres <- f(h)
    tres <- traverseViaFor(t)(f)
  } yield fres :: tres
}
def sequenceViaTraverseViaFor[A](a: List[Option[A]]): Option[List[A]] =
  traverseViaFor(a)(identity)
Some(List(1,2,3)) == sequenceViaTraverseViaFor(List(Some(1), Some(2), Some(3)))
None == sequenceViaTraverseViaFor(List(Some(1), None, Some(3)))
