// Exercise 7
case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]
sealed trait Either[+E, +A] {
//  def map[B](f: A => B): Either[E, B] = this match {
//    case Right(v) => Right(f(v))
//    case Left(v) => Left(v)
//  }
  def map[B](f: A => B): Either[E, B] =
    flatMap(a => Right(f(a)))
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(v) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
    //this.flatMap(aa => b.map(bb => f(aa, bb)))
  /*
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this,b) match {
    case (Left(l1), Left(l2)) => Left(List(l1, l2))
    case (Left(l), Right(r)) => Left(l)
    case (Right(r), Left(l)) => Left(List(l))
    case (Right(r1), Right(r2)) => Right(f(r1, r2))
  }
  */
}
// Exercise 8
def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
  case Nil => Right(Nil)
  case h :: t => for {
    hh <- f(h)
    tt <- traverse(t)(f)
  } yield hh :: tt
  //f(h).map2(traverse(t)(f))(_ :: _)
}
/*
def sequence[A, B](a: List[Either[A, B]]): Either[A, List[B]] = a match {
  case Nil => Right(Nil)
  case Left(h: A) :: _ => Left(h)
  case Right(h: B) :: t => sequence(t).flatMap((x: List[B]) => Right[List[B]](h :: x) )
  case Right(h: B) :: _ => Right(List(h))
}
Right(List("a", 2)) == sequence(List(Right("a"), Right(2)))
Right(List(1)) == sequence(List(Right(1)))
Right(List(1)) == sequence(List(Right(???)))
*/
def sequence[A, B](a: List[Either[A, B]]): Either[A, List[B]] = a match {
  case Nil => Right(Nil)
  case h :: t => for {
    hh <- h
    tt <- sequence(t)
  } yield hh :: tt
}
Right(List("a", 2)) == sequence(List(Right("a"), Right(2)))
Right(List(1)) == sequence(List(Right(1)))
Left("error") == sequence(List(Left("error"), Right("something")))

def sequenceViaTraverse[A, B](a: List[Either[A, B]]): Either[A, List[B]] =
  traverse(a)(identity)
Right(List("a", 2)) == sequenceViaTraverse(List(Right("a"), Right(2)))
Right(List(1)) == sequenceViaTraverse(List(Right(1)))
Left("error") == sequenceViaTraverse(List(Left("error"), Right("something")))


case class Person(name: Name, age: Age)
case class Name(val value: String)
case class Age(val value: Int)
def map2[EE, B, C, A](a: Either[EE, A], b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] =
  (a,b) match{
    case (Left(l1), Left(l2)) => Left(List(l1, l2))
    case (Left(l), Right(r)) => Left(List(l))
    case (Right(r), Left(l)) => Left(List(l))
    case (Right(r1), Right(r2)) => Right(f(r1, r2))
  }
def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))
def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))
def mkPerson(name: String, age: Int): Either[List[String], Person] =
  map2(mkName(name), mkAge(age))(Person(_, _))
  //mkName(name).map2(mkAge(age))(Person(_, _))
Left(List("Name is empty.", "Age is out of range.")) == mkPerson("", -1)
Left(List("Age is out of range.")) == mkPerson("a", -1)
Left(List("Name is empty.")) == mkPerson("", 1)
Right(Person(Name("a"), Age(1))) == mkPerson("a", 1)