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
  def getOrElse[B >: A](default: => B): B = default
  def orElse[B >: A](ob: => Option[B]): Option[B] = ob
  def filter(f: A => Boolean): Option[A] = if (f(v)) Some(v) else None
}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty)
    None
  else
    Some(xs.sum / xs.size)

def variance(xs: Seq[Double]): Option[Double] = {
  val mo = mean(xs)
  mo.flatMap(mm =>
    mean(xs.map(x =>
      math.pow(x - mm, 2))))
}
None == variance(List())
2/3 == variance(List(1,2,3)).getOrElse(0)
