// Exercise 7
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
trait Either[+E, +A] {
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
