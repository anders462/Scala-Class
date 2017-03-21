package week4


trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] { //head and tail already impl due to val def
def isEmpty = false
}

class Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

/**
  * Created by andersbengtsson on 2/24/17.
  */
object List {
  // List(1, 2) = List.apply(1 ,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](): List[T] = new Nil

}

object test {
  val x = List[String] = Nil
}
