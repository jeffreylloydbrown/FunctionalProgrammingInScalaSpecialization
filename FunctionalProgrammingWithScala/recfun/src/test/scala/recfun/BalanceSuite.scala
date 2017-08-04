package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance
  import Main.balanceWorker
  import Main.checkForParen

  // Making sure character constants work in Scala like in C++.
  test("checkForParen('(') is 1") {
    assert(checkForParen('(') === +1)
  }

  test("checkForParen(')') is -1") {
    assert(checkForParen(')') === -1)
  }

  test("checkForParen('') is 0") {
    assert(checkForParen('\u0000') === 0)
  }

  test("checkForParen('a') is 0") {
    assert(checkForParen('a') === 0)
  }

  // Instructor-required tests.
  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  // A few more of my own.
  test("balance: empty string is balanced") {
    assert(balance("".toList))
  }

  test("balance: string with no parens is balanced") {
    assert(balance("abc".toList))
  }

  test("balance: '()()' is balanced") {
    assert(balance("()()".toList))
  }

  test("balance:  '(())' is balanced") {
    assert(balance("(())".toList))
  }

  test("balance: ')()(' is unbalanced") {
    assert(!balance(")()(".toList))
  }

  test("balance: '((())))()' is unbalanced") {
    assert(!balance("((())))()".toList))
  }

}
