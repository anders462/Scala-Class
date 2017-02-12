package funsets

object Main extends App {
  import FunSets._
  val s = Set(200, 300, 400)
  val s2 = Set(500, 600, 300)
  val t = singletonSet(2)
  val p = (x:Int) => x > 1000
  val f = (x:Int) => x * 2
  println(union(s,t)(4))
  println(intersect(s,t)(1))
  println(diff(s,t)(1))
  println(filter(s,p)(1))
  println(forall(s,p))
  println(exists(s,p))
  printSet(map(s, f))
  printSet(diff(s,s2))
}
