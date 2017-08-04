package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  import Main.validateCoinList

  // confirm coin list validation
  test("valid coin list returns true") {
    assert(validateCoinList(List(5,10,1,25)))
  }

  test("negative coin value throws exception") {
    intercept[java.lang.IllegalArgumentException] {
      validateCoinList(List(5,1,-5))
    }
  }

  test("zero coin value throws exception") {
    intercept[java.lang.IllegalArgumentException] {
      validateCoinList(List(5,0))
    }
  }

  // Degenerate cases.  Illegal parameter, no money, no coins.
  test("money cannot be negative") {
    intercept[java.lang.IllegalArgumentException] {
      countChange(-100, List(1,2))
    }
  }

  test("money == 0 means no change") {
    assert(countChange(0, List(1,2,5)) === 0)
  }

  test("no coins means no change") {
    assert(countChange(200, List()) === 0)
  }

  test("all coins are larger than money so no change") {
    assert(countChange(20, List(25,50,100)) === 0)
  }

  // Instructor-required tests.
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  // Some more from me.

  test("countChange:  1 coin means 1 way iff it divides") {
    assert(countChange(4,List(2)) === 1)
  }

  test("countChange: 1 coin means 0 ways iff it doesn't divide") {
    assert(countChange(4,List(3)) === 0)
  }

}
