object rationals {


  //Rational data type
  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be non zero")

    def this(x: Int) = this(x, 1); //***

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    def numer: Int = x / g
    def denom: Int = y / g


    //******* class methods *******//
    def + (that: Rational) =
      new Rational(
        this.numer * that.denom + that.numer * this.denom,
        denom * that.denom)

    override def toString = {
  //    val g = gcd(this.numer, this.denom) //****
      this.numer/g + "/" + this.denom/g
    }

    def unary_- : Rational = new Rational(-this.numer, this.denom)

    def - (that: Rational) = this + -that  // + and - are symbols

    def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (this < that) that else this  // < is symbol


  }

//** if called often use val numer = x / g
//*** secondary constructor
//**** could do simplification here but will result in errors as numbers get larger

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
//
  x.+(y)
  x - y -z
  y + y // same as y.add(y)
  assert(x < y)
  x max y //same as a.max(y)










  //using functions
  def addRational(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom
    )


  def makeString(r: Rational): String =
    r.numer + "/" + r.denom

  makeString( addRational(new Rational(1,2), new Rational(2,3)) )

}


