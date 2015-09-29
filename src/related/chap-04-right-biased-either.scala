object either {
  import scala.{Either => _, Left => _, Right => _}

  trait Either[+A, +B] {

    def fold[X](fa: A => X, fb: B => X) = (this: @unchecked) match {
      case Left(a) => fa(a)
      case Right(b) => fb(b)
    }

    def swap = (this: @unchecked) match {
      case Left(a) => Right(a)
      case Right(b) => Left(b)
    }

    def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] =
      (this: @unchecked) match {
        case Left(a)  => Left(a)
        case Right(b) => b
      }

    def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] =
      (this: @unchecked) match {
        case Left(a)  => a
        case Right(b) => Right(b)
      }

    def isLeft: Boolean

    def isRight: Boolean

    def get = this match {
      case Left(_) => throw new NoSuchElementException("Left.get")
      case Right(b) => b
    }

    def getOrElse[BB >: B](or: => BB) = this match {
      case Left(_) => or
      case Right(b) => b
    }

    def foreach[U](f: B => U) = this match {
      case Left(_) => {}
      case Right(b) => f(b)
    }

    def forall(f: B => Boolean) = this match {
      case Left(_) => true
      case Right(b) => f(b)
    }

    def exists(f: B => Boolean) = this match {
      case Left(_) => false
      case Right(b) => f(b)
    }

    def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = this match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }

    def map[Y](f: B => Y) = this match {
      case Left(a) => Left(a)
      case Right(b) => Right(f(b))
    }

    def toSeq = this match {
      case Left(_) => Seq.empty
      case Right(b) => Seq(b)
    }

    def toOption = this match {
      case Left(_) => scala.None
      case Right(b) => Some(b)
    }
  }

  case class Left[+A, +B](a: A) extends Either[A, B] {
    def isLeft = true
    def isRight = false
  }

  case class Right[+A, +B](b: B) extends Either[A, B] {
    def isLeft = false
    def isRight = true
  }

  object Either {
    class MergeableEither[A](x: Either[A, A]) {
      def merge: A = (x: @unchecked) match {
        case Left(a)  => a
        case Right(a) => a
      }
    }

    implicit def either2mergeable[A](x: Either[A, A]): MergeableEither[A] = new MergeableEither(x)
  }

  object tests {
    object Sobriety extends Enumeration {
      val Sober, Tipsy, Drunk, Paralytic, Unconscious = Value
    }

    object Gender extends Enumeration {
      val Male, Female = Value
    }

    case class Person(gender: Gender.Value, age: Int, sobriety: Sobriety.Value)

    object people {
      val Ken = Person(Gender.Male, 28, Sobriety.Tipsy)
      val Dave = Person(Gender.Male, 41, Sobriety.Sober)
      val Ruby = Person(Gender.Female, 25, Sobriety.Tipsy)
    }

    def checkAge(p: Person): Either[String, Person] =
      if (p.age < 18) Left("Too Young!")
      else if (p.age > 40) Left("Too Old!")
      else Right(p)

    def checkSobriety(p: Person): Either[String, Person] =
    if (Set(Sobriety.Drunk, Sobriety.Paralytic, Sobriety.Unconscious) contains p.sobriety) Left("Sober Up!")
    else Right(p)

    def bribingCosts(p: Person): Either[String, Double] = {
      for {
        a <- checkAge(p)
        b <- checkSobriety(a)
        amount = if (b.gender == Gender.Female) 0D else 5D
      } yield amount
    }
  }
}

