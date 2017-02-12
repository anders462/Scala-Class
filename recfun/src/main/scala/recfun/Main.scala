package recfun

object Main {
  def main(args: Array[String]) {
//        println("Pascal's Triangle")
//        for (row <- 0 to 10) {
//          for (col <- 0 to row)
//            print(pascal(col, row) + " ")
//          println()
//        }
//    def cube = (x: Int) => x*x*x
////    val sumCube = sumC(cube)
//    print(sumCs(cube)(1, 3))

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def charCheck(open: Int, chars: List[Char]): Boolean =
      if (chars.nonEmpty && !(open < 0)) {
        var count = open
        if (chars.head == 40) count += 1
        if (chars.head == 41) count -= 1
        charCheck(count, chars.tail)
      } else open == 0

    charCheck(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    print(money, coins)
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }

  /**
    * Higher order functions
    */

  def sumRec(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumRec(f, a + 1, b)
  }


  def sum(f: Int => Int)( a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  /**
    * Currying Higher order functions
    */

  def sumC(f: Int => Int):(Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sumRec(f, a + 1, b)
    sumF
  }

  /**
    * Syntatic sugar version of above
    */

  def sumCs(f: Int => Int)(a: Int, b: Int):Int =
      if (a > b) 0
      else f(a) + sum(f)( a + 1, b)

  /**
    * Product: applies f to all values a,b and take prod
    */

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

//  print("product :" + product(x => x*x)(3, 4))

  /**
    * factorial: uses product to calc factorial
    * 1 start point and n end point
    */

  def fact(n: Int): Int = product(n => n)(1, n)

//  print("factorial: " + fact(5))

  /**
    * generalise: general solution for both sum and prod
    */

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero )(a + 1, b))

//  print("mapReduce: " + mapReduce(x => x, (a, b) => a * b, 1)(1, 5))

  def newProd(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(x => x, (a, b) => a * b, 1)(a, b)

  print("New product: " + newProd(x => x)(1,5))

}
