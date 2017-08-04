package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  // Exceptions cases.  The spec should have said unsigned integers, since negative
  // row and column aren't allowed.  But it didn't so I have to check it.
  // Column cannot ever be larger than row, either.
  test("row cannot be negative") {
    intercept[java.lang.IllegalArgumentException] {
      pascal(0,-1)
    }
  }

  test("column cannot be negative") {
    intercept[java.lang.IllegalArgumentException] {
      pascal(-1,0)
    }
  }

  test("column cannot be > row") {
    intercept[java.lang.IllegalArgumentException] {
      pascal(4,3)
    }
  }

  // Non-recursive cases.  Anything in column 0 is 1, anything where
  // column and row are the same is 1.
  test("pascal: col=0,row=0 should be 1") {
    assert(pascal(0,0) === 1)
  }

  test("pascal: col=0,row=1 should be 1") {
    assert(pascal(0,1) === 1)
  }

  test("pascal: col=1,row=1 should be 1") {
    assert(pascal(1,1) === 1)
  }

  test("pascal: col=5,row=5 should be 1") {
    assert(pascal(5,5) === 1)
  }

  // Instructor provided 3 required tests.
  test("pascal: col=0,row=2 should be 1") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2 should be 2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3 should be 3") {
    assert(pascal(1,3) === 3)
  }

  // Add a few of my own.
  test("pascal: col=2,row=4 should be 6") {
    assert(pascal(2,4) === 6)
  }

  test("pascal: col=3,row=5 should be 10") {
    assert(pascal(3,5) === 10)
  }
}
