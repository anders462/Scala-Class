import math.Ordering

object week5 {

  def removeAt(n: Int, xs: List[Int]): List[Int] = (xs take n) ++ (xs drop n + 1)

  val a = List(1,2,3,5,6)

  removeAt(3, a)

//Flatten lists

  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case List() => List()
      case (y :: ys) :: yss => flatten(y :: ys) ::: flatten(yss)
      case y :: ys => y :: flatten(ys)
    }
  }

  flatten(List(List(1, 1), 59, List(3, List(10, 8))))

  //Sorting algorithms for

  // 1 Merge Sort (divide and conquer)

//  def mergeL(xs: List[Int], ys: List[Int]): List[Int] = {
//    xs match {
//      case Nil => ys
//      case x :: xs1 =>
//        ys match {
//          case Nil => xs
//          case y :: ys1 =>
//            if (x < y) x :: mergeL(xs1, ys)
//            else y :: mergeL(xs, ys1)
//        }
//    }
//  }

  import math.Ordering
  //replace lt: (T, T) => Boolean with general class Ordering

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length/2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)

      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))

    }
  }

  val num = List(3,6,86,89,3,8,490)
  val strings = List("plllkkkhhafj", "jfjj", "sjdjd", "ajdnllllllll","ag")

msort(strings)


  def scaleList(xs: List[Int], factor: Int): List[Int] = {
    xs match {
      case Nil => xs
      case y :: ys => y * factor :: scaleList(ys, factor)
    }
  }

  val listA = List(1, 14, 5)
  scaleList(listA, 3)


  //simple implementation of map, nor tail recursive
//  abstract class List[T] {
//    def map[U](f: T => U): List[U] = this match {
//      case Nil => this
//      case x :: xs => f(x) :: xs.map(f)
//    }
//  }

  //using map
  def squareList(xs: List[Int]): List[Int] = xs map (x => x * x)

  squareList(List(2,3,5))

  //using pattern match
  def squareListM(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareListM(ys)
  }

  squareListM(List(2,3,5))

  

}

