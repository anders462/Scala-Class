package week4

/**
  * Created by andersbengtsson on 2/24/17.
  */
abstract class Nat {
  def isZero: Boolean
  def successor: Nat = new Succ(this)
  def predecessor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

// Zero can be singleton
object Zero extends Nat {
  def isZero = true
  def predecessor = throw new java.util.NoSuchElementException("Integer zero")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}


