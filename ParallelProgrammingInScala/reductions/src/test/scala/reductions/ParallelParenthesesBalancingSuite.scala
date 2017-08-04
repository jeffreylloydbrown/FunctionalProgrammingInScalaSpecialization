package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
    check("ab", true)
  }

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toArray) === true)
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray) === true)
  }

  test("balance: ':-)' is unbalanced") {
    assert(balance(":-)".toArray) === false)
  }

  test("balance: counting is not enough") {
    assert(balance("())(".toArray) === false)
  }

  // Force traverse() to be called by using an absurd threshold.

  test("force traverse: balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1000) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("force traverse: balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1000) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("force traverse: balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1000) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
    check("ab", true)
  }

  test("force traverse: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(parBalance("(if (zero? x) max (/ 1 x))".toArray, 1000) === true)
  }

  test("force traverse: 'I told him ...' is balanced") {
    assert(parBalance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray, 1000) === true)
  }

  test("force traverse: ':-)' is unbalanced") {
    assert(parBalance(":-)".toArray, 1000) === false)
  }

  test("force traverse: counting is not enough") {
    assert(parBalance("())(".toArray, 1000) === false)
  }

  test("force traverse: '()()' is balanced") {
    assert(parBalance("()()".toArray, 1000) === true)
  }

  test("force traverse: '(())' is balanced") {
    assert(parBalance("(())".toArray, 1000) === true)
  }

  test("force traverse: ')()(' is unbalanced") {
    assert(parBalance(")()(".toArray, 1000) === false)
  }

  test("force traverse: '((())))()' is unbalanced") {
    assert(parBalance("((())))()".toArray, 1000) === false)
  }

  // Force reduce() to run parallel tasks.

  test("reduce: balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, input.length/3) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("reduce: balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, input.length/3) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("reduce: balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, input.length/3) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
    check("ab", true)
  }

  test("reduce: '(if (zero? x) max (/ 1 x))' is balanced") {
    val res = parBalance("(if (zero? x) max (/ 1 x))".toArray, 4)
    assert(parBalance("(if (zero? x) max (/ 1 x))".toArray, 4) === true)
  }

  test("reduce: 'I told him ...' is balanced") {
    assert(parBalance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray, 8) === true)
  }

  test("reduce: ':-)' is unbalanced") {
    assert(parBalance(":-)".toArray, 2) === false)
  }

  test("reduce: counting is not enough") {
    assert(parBalance("())(".toArray, 2) === false)
  }

  test("reduce: '()()' is balanced") {
    assert(parBalance("()()".toArray, 2) === true)
  }

  test("reduce: '(())' is balanced") {
    assert(parBalance("(())".toArray, 2) === true)
  }

  test("reduce: ')()(' is unbalanced") {
    assert(parBalance(")()(".toArray, 1) === false)
  }

  test("reduce: '((())))()' is unbalanced") {
    assert(parBalance("((())))()".toArray, 2) === false)
  }


}