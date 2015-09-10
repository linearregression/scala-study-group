import scala.annotation.tailrec
@tailrec
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  if (as.length <= 1) {
    true
  } else {
    val tail = as.slice(1, as.length)
    if (!gt(as(0), as(1)))
      false
    else
      isSorted(tail, gt)
  }
}

//def gt[A](l:A, r:A) = l == r
//val f > eq(_, _)
val greater_than = (a: Int, b: Int) => a >= b
false == isSorted(Array(4, 2, 3), greater_than)
true == isSorted(Array(3, 2, 1), greater_than)
true == isSorted(Array(3, 1, 1), greater_than)
true == isSorted(Array(3), greater_than)
true == isSorted(Array(), greater_than)
def partial[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}
val gt5 = partial(5, greater_than)
gt5(4) == true
gt5(6) == false

def myFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  l.reverse.foldRight(z)((n,m)=>f(m,n))
}

myFoldLeft(List("aa","ab"), "")((aa,bb)=> bb.reverse + aa)
List("aa","ab").foldLeft("")((bb,aa)=> aa.reverse + bb)