// Define Tree
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => size(l) + size(r)
}
1 == size(Leaf(10))
2 == size(Branch(Leaf(10), Leaf(20)))
3 == size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
def maximum(t: Tree[Int]): Int = t match {
  case Leaf(x) => x
  case Branch(l, r) => maximum(l) max maximum(r)
}
10 == maximum(Leaf(10))
20 == maximum(Branch(Leaf(10), Leaf(20)))
2 == maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(0)))
def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}
1 == depth(Leaf(10))
2 == depth(Branch(Leaf(10), Leaf(20)))
3 == depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(0)))
def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
}
Branch(Branch(Leaf(2), Leaf(3)), Leaf(1)) == map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(0)))(_ + 1)
def fold[A, B](t: Tree[A])(handleLeaf: A => B)(handleBranch: (B, B) => B): B = t match {
  case Leaf(x) => handleLeaf(x)
  case Branch(l, r) =>
    val left = fold(l)(handleLeaf)(handleBranch)
    val right = fold(r)(handleLeaf)(handleBranch)
    handleBranch(left, right)
}
def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(x => 1)((a, b) => a + b)
1 == sizeViaFold(Leaf(10))
2 == sizeViaFold(Branch(Leaf(10), Leaf(20)))
3 == sizeViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(x => 1)((l, r) => 1 + (l max r))
1 == depthViaFold(Leaf(10))
2 == depthViaFold(Branch(Leaf(10), Leaf(20)))
3 == depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(0)))

def maximumViaFold(t: Tree[Int]): Int =
  fold(t)(x => x)((l, r) => l max r)
10 == maximumViaFold(Leaf(10))
20 == maximumViaFold(Branch(Leaf(10), Leaf(20)))
2 == maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(0)))

def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
Leaf(2) == mapViaFold(Leaf(1))(_ + 1)
Branch(Leaf(3), Leaf(4)) == mapViaFold(Branch(Leaf(2), Leaf(3)))(_ + 1)
Branch(Leaf(3), Branch(Leaf(2), Leaf(2))) == mapViaFold(Branch(Leaf(2), Branch(Leaf(1), Leaf(1))))(_ + 1)
