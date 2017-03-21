object listfun {


  val num = List(12,56,86,89,3,8,490)
  val strings = List("plllkkkhhafj", "jfjj", "sjdjd", "ajdnllllllll","ag")

  num filter (x => x > 10)
  num filterNot (x => x > 10)
  num partition (x => x > 10)

  num takeWhile (x => x > 10)
  num dropWhile (x => x > 10)
  num span (x => x > 10)



 // List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }


  pack(List("a", "a", "a", "b", "c", "c", "a"))

  val text = List("a", "a", "a", "b", "c", "c", "a")


  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (ys => (ys.head, ys.length))

  }

  encode(text)

//  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
//    (xs foldRight List[U]())( ??? )
//
//  def lengthFun[T](xs: List[T]): Int =
//    (xs foldRight 0)( ??? )pro

  def sum(xs: List[Int]) = (0 :: xs) reduceLeft (_+_)

  sum(num)

  def prod(xs: List[Int]) = (1 :: xs) reduceLeft (_*_)

  prod(List(3,3,3))

  def sumF(xs: List[Int]) = (xs foldLeft 0) (_+_)

  sumF(num)


  def prodF(xs: List[Int]) = (xs foldLeft 1) (_*_)

  prodF(List(3,3,3))


  def sumRr(xs: List[Int]) = xs reduceRight (_+_)

  sumRr(num)

  def sumFr(xs: List[Int]) = (xs foldRight 3)((x,y) => x + y)

  sumFr(num)


  def mapFunL[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldLeft List[U]())((ys, x) => f(x)::ys)


  mapFunL(num, (x: Int) => 2*x)


  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x, ys) => f(x)::ys)

  mapFun(num, (x: Int) => 2*x)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (_, y) => 1 + y )

  lengthFun(num)

//  num ++ num

  val a = Vector(1,2,3,4)


  val b: Range = 0 until 5
  val c: Range = 1 to 10 by 3

 b exists (x => x > 2)

 6 +: a

a :+ 10

a filter (x => x > 2)

a map (x => x*2)
  
  val st: Range = 1 to 60



}