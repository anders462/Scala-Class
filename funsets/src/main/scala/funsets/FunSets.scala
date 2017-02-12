package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
    * JS >> const Set = (elem) => set => set === elem
   */
    def singletonSet(elem: Int): Set = set => set == elem
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
    * s = Set(1) and t = Set(2) => Set()
   */
    def union(s: Set, t: Set): Set = (elem: Int) => s(elem) || t(elem)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = (elem: Int) => s(elem) && t(elem)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = (elem: Int) => s(elem) &&  !t(elem)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = (elem: Int) => contains(s, elem) && p(elem)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !(s(a) && p(a))) false
      else iter(a + 1)
    }
    iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
    * for all returns true if all element satisfies p
    * forall(s, x=> !p(x)) returns true if no element satisfies p
    * !forall(s, x=> !p(x)) returns true if any element satisfies
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))


//  def exists(s: Set, p: Int => Boolean): Boolean = {
//    def iter(a: Int): Boolean = {
//      if (a > bound) false
//      else if (contains(s, a) && filter(s, p)(a)) true
//      else iter(a + 1)
//    }
//    iter(-bound)
//  }

//  def map(s: Set, f: Int => Int): Set = (e: Int) => exists(s, x => f(x) == e)
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */

  def map(s: Set, f: Int => Int): Set = (elem: Int) => exists(s, x => elem == f(x))

//  def map(s: Set, f: Int => Int): Set = {
//      def iter(a: Int, b: Set): Set = {
//        if (a < -bound) b
//        else if(contains(s, a)) iter(a-1, union(b, singletonSet(f(a))))
//        else iter(a-1, b)
//      }
//      iter(bound, Set())
//    }

  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
