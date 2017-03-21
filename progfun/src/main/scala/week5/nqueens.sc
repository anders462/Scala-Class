

def queens(n: Int): Set[List[Int]] = {

  def placeQueen(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
    for {
      queens <- placeQueen(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens



  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row -1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  placeQueen(n)
}

val q = queens(4)

def show(queens: List[Int]): String = {
  val lines =
    for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X").mkString
    "\n" + (lines mkString  "\n")
}

Vector.fill(5)("* ").mkString

(queens(8) take 3 map show) mkString "\n"


class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int,Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
 def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
 override def toString =
   (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2, 3 -> 4, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3, 3 -> 7))

p1 + p2

class Poly2(val terms0: Map[Int, Double]) {
  def this(bindings: (Int,Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly((other.terms foldLeft terms))(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }






  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

