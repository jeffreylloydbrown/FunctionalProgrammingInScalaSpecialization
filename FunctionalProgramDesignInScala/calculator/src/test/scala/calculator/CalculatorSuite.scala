package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() === MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() === MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() === MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() === "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() === "green")
    val resultGreen3 = TweetLength.colorForRemainingCharsCount(Var(MaxTweetLength + 1))
    assert(resultGreen3() === "green")
    val resultGreen4 = TweetLength.colorForRemainingCharsCount(Var(MaxTweetLength))
    assert(resultGreen4() === "green")
    val resultGreen5 = TweetLength.colorForRemainingCharsCount(Var(MaxTweetLength - 1))
    assert(resultGreen5() === "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() === "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() === "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() === "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() === "red")
  }

  import Polynomial._
  test("discriminant(x^2 - 6x + 9) == 0") {
    assert(computeDelta(Signal(1), Signal(-6), Signal(9))() === 0)
  }

  test("discriminant(x^2 - 9) == 36") {
    assert(computeDelta(Signal(1), Signal(0), Signal(-9))() === 36)
  }

  test("discriminant(x^2 - x + 1) == -3") {
    assert(computeDelta(Signal(1), Signal(-1), Signal(+1))() === -3)
  }

  test("solution(x^2 - 6x + 9) = Set(3)") {
    val a = Signal(1.0)
    val b = Signal(-6.0)
    val c = Signal(9.0)
    val delta = computeDelta(a, b, c)
    assert(computeSolutions(a, b, c, delta)() === Set(3))
  }

  test("solution(x^2 - 9) = Set(-3, 3)") {
    val a = Signal(1.0)
    val b = Signal(0.0)
    val c = Signal(-9.0)
    val delta = computeDelta(a, b, c)
    assert(computeSolutions(a, b, c, delta)() === Set(-3, 3))
  }

  test("solution(x^2 - x + 1) = Set()") {
    val a = Signal(1.0)
    val b = Signal(-1.0)
    val c = Signal(1.0)
    val delta = computeDelta(a, b, c)
    assert(computeSolutions(a, b, c, delta)() === Set())
  }

  import Calculator._

  val refs = Map[String, Signal[Expr]](
                                        "a" -> Signal(Literal(1000)),
                                        "b" -> Signal(Plus(Literal(2), Literal(5))),
                                        "c" -> Signal(Minus(Literal(2), Literal(5))),
                                        "d" -> Signal(Times(Literal(2), Literal(5))),
                                        "e" -> Signal(Divide(Literal(-15), Literal(-3))),
                                        "f" -> Signal(Divide(Literal(2), Literal(0))),
                                        "g" -> Signal(Times(Ref("a"), Ref("b"))),
                                        "h" -> Signal(Minus(Ref("g"), Ref("f"))),
                                        "i" -> Signal(Divide(Ref("f"), Ref("b"))),
                                        "j" -> Signal(Plus(Ref("k"), Literal(1))),
                                        "k" -> Signal(Times(Literal(2), Ref("j")))
                                        )

  test("eval(Literal(value)) == value") { assert(eval(Literal(34), refs) === 34) }

  test("eval(Literal(2) + Literal(5)) == 7") { assert(eval(Plus(Literal(2), Literal(5)), refs) === 7) }

  test("eval(Literal(2) - Literal(5)) == -3") { assert(eval(Minus(Literal(2), Literal(5)), refs) === -3) }

  test("eval(Literal(2) * Literal(5)) == 10") { assert(eval(Times(Literal(2), Literal(5)), refs) === 10) }

  test("eval(Literal(-15) / Literal(-5)) == 3") { assert(eval(Divide(Literal(-15), Literal(-5)), refs) === 3) }

  test("eval(Literal(2) / Literal(0)) == Double.NaN") {
    assert(eval(Divide(Literal(2), Literal(0)), refs).toString === Double.NaN.toString)
  }

  test("eval(e) == 5") { assert(eval(Ref("e"), refs) === 5) }

  test("eval(g) === 7000") { assert(eval(Ref("g"), refs) === 7000) }

  test("eval(h) == NaN") { assert(eval(Ref("h"), refs).toString === Double.NaN.toString) }

  test("eval(z) === NaN") { assert(eval(Ref("z"), refs).toString === Double.NaN.toString) }

  test("j and k are circular references") {
    assert(eval(Ref("k"), refs).toString === Double.NaN.toString)
    assert(eval(Ref("j"), refs).toString === Double.NaN.toString)
  }

  test("computeValues on empty ref map is empty result map") {
    val expressions = Map[String, Signal[Expr]]()
    val empty_results = Map[String, Signal[Double]]()
    assert(computeValues(expressions) === empty_results)
  }

  test("run computeValues on test refs") {
    // Take a results map and convert it into a sorted list of pairs so we can use assertions on the result.
    // Because Double.NaN could be present, make all the result values into strings.
    def noSignals(m: Map[String, Signal[Double]]): List[(String, String)] = {
      ( for ((k, v) <- m) yield (k, v().toString) ).toList.sortWith(_._1 < _._1)
    }

    val expected: Map[String, Signal[Double]] = Map(
                        "a" -> Signal(1000),
                        "b" -> Signal(7),
                        "c" -> Signal(-3),
                        "d" -> Signal(10),
                        "e" -> Signal(5),
                        "f" -> Signal(Double.NaN),
                        "g" -> Signal(7000),
                        "h" -> Signal(Double.NaN),
                        "i" -> Signal(Double.NaN),
                        "j" -> Signal(Double.NaN),
                        "k" -> Signal(Double.NaN)
                      )

    assert(noSignals(computeValues(refs)) === noSignals(expected))
  }

}
