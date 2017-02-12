package recfun

/**
  * Created by andersbengtsson on 2/4/17.
  */
import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SumSuite extends FunSuite {
  import Main.sum

  test("sum: sum adds up to 78") {
    assert(sum(x => x)(1, 12) === 78)
  }

  test("sum: sum adds up to 14") {
    assert(sum(x => x*x)(1, 3) === 14)
  }
}
