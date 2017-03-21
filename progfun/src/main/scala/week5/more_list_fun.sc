val a = List(1,2,3,5,6,7)
val b = List(10,11,12,13,14)
val hello = List("h", "e", "l", "l", "o")

a exists (x => x == 6)
a forall (x => x < 3)
val c = a zip b
c unzip
val s = "hello world"

s flatMap (c => List(".", c))

val matrix = List(List(1,2,3), List(4,5,6), List(7,8,9))

val f = matrix flatMap (x => x)

f.sum
f.product
f.max
f.min

def perm(n: Int, m: Int) =
  1 to n flatMap (x => 1 to n map (y => (x,y)))

perm(4, 5)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]) =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

scalarProduct(Vector(1,2,3), Vector(3,4,5))

def scalarProductAlt(xs: Vector[Double], ys: Vector[Double]) =
  (xs zip ys).map{case (x,y) => x*y}.sum

scalarProductAlt(Vector(1,2,3), Vector(3,4,5))

def scalarProductFor(xs: Vector[Double], ys: Vector[Double]) =
  (for {
    (x, y) <- xs zip ys

  } yield x * y).sum

scalarProductFor(Vector(1,2,3), Vector(3,4,5))


def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

isPrime(11)


val n = 7

((1 until n) map (i =>
  (1 until i) map (j => (i, j)))).flatten

((1 until n) flatMap (i =>
  (1 until i) map (j => (i, j)))) filter({case (x, y) => isPrime(x + y)})

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)


val xs = List(List(1, 6),List(3,4))


val nums = List(1,2,3,4,5,6,7,8,9)


for (num <- nums if num > 3) yield num





def queens(n: Int): Set[List[Int]] = {

  def placeQueen(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueen(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueen(n)

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row -1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }
}

queens(4)