package week3

/**
  * Created by andersbengtsson on 2/16/17.
  */
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
