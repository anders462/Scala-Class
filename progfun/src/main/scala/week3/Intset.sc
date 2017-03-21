
/**
  * Created by andersbengtsson on 2/15/17.
  */
object Intset {

  abstract class Intset {
    def incl(x: Int): Intset
    def contains(x: Int): Boolean
    def union(other: Intset): Intset
  }

  object Empty extends Intset {
    def contains(x: Int): Boolean = false
    def incl(x: Int): Intset = new NonEmpty(x: Int, Empty, Empty)
    def union(other: Intset): Intset = other
    override def toString = "."

  }

  class NonEmpty(elem: Int, left: Intset, right: Intset) extends Intset {

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): Intset =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def union(other: Intset): Intset =
      ((left union right) union other) incl elem

    override def toString = "{" + left + elem + right + "}"

  }

  val t1 = new NonEmpty(3, Empty, Empty)
  val t6 = new NonEmpty(8, Empty, Empty)
  val t2 = t1 incl 4
  val t3 = t2 incl 5
  val t4 = t3 incl 1
  val t5 = t1 union t6 union t2

  t2 toString


}