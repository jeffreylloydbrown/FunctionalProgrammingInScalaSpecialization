package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    * @param chars      An array representing a string, usually created with "my string".toArray .
    * @return           `true` if `chars` has balanced parentheses.  "a" is balanced.  "()" is balanced.
    *                   ")(" is not balanced even though the left and right paren count is the same.
    **/
  def balance(chars: Array[Char]): Boolean = balanceWorker(0, 0, chars, 0, chars.length)

  /** My Boolean.toInt conversion.  Returns 1 if `e` is `true` and 0 if `e` is `false`.
    */
  private def toInt(e: Boolean): Int = if (e) 1 else 0

  // balanceWorker is the guts of the balance check.  It steps through the array looking
  // for parens.  Left parens increase `left`, right parens increase `right`.  If `right` > `left`
  // we know we aren't balanced so answer `false`.  If we get to the end of the array
  // and `left` == `right`, we are balanced and return `true`; if `left` and `right` differ
  // then we're unbalanced so return `false`.
  private def balanceWorker(left: Int, right: Int, chars: Array[Char], cur: Int, end: Int): Boolean = {
    if (right > left) false
    else if (cur >= end) (left == right)    // notice that is a Boolean expression!
    else
      balanceWorker(left + toInt( chars(cur) == '(' ), right + toInt( chars(cur) == ')' ), chars, cur + 1, end)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    * @param chars      An array representing a string, usually created with "my string".toArray .
    * @param threshold  String segments shorter than `threshold` are processed sequentially instead of in parallel.
    * @return           `true` if `chars` has balanced parentheses.  "a" is balanced.  "()" is balanced.
    *                   ")(" is not balanced even though the left and right paren count is the same.
    **/
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /** Walk the `chars` array from `idx` to `until` (exclusive) sequentially (without spawning threads).
      * Bump the open (left) paren count for every '(' seen.  When ')' seen, if we have an unbalanced '(' then
      * lower the open (left) paren count.  Otherwise, bump the close (right) paren count.
      *
      * @param idx      The starting point of the string segment.  Inclusive.
      * @param until    The ending point.  Exclusive.
      * @param open     The unbalanced '(' count.
      * @param close    The unbalanced ')' count.
      * @return         A pair (`open`, `close`) representing the unbalanced counts for this segment.
      **/
    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx >= until) (open, close)
      else
        chars(idx) match {
          case '('    => traverse(idx+1, until, open+1, close)
          case ')'    =>
            if (open > 0) traverse(idx+1, until, open-1, close)  // we have a paren we can "balance" away
            else          traverse(idx+1, until, open, close+1)
          case _      => traverse(idx+1, until, open, close)
        }
    }

    /** Process the `chars` array starting at `from` up to `until`.  If the segment length is longer than
      * `threshold`, split the segment into 2 roughly equal pieces and process each in parallel.  If they are
      * shorter than `threshold`, or if `threshold` is stupid (zero or negative), process sequentially.
      *
      * @param from     Starting point in `chars`, inclusive.
      * @param until    Ending point of the segment, exclusive.
      * @return         A pair (`open`, `close`) representing the unbalanced counts for this segment.
      **/
    def reduce(from: Int, until: Int): (Int, Int) = {
      if ( (until - from) <= threshold  || threshold <= 0 ) traverse(from, until, 0, 0)
      else
        {
          val mid = from + ( (until - from)/2 )
          val ( (open_l, close_l), (open_r, close_r) ) = parallel(reduce(from, mid), reduce(mid, until))

          // The "tricky" part is how to combine the 4 returned values together into a pair of values.
          // Took some whiteboard work to figure out.  How you combine depends on if there are more open
          // parens than close parens.  If open is bigger, use `rr` to "balance" away what we can.  Then
          // include the remaining open parens in `rl`.  Similarly, if there are more close parens, use `ll` to
          // "balance" away what we can then include the remaining close parens in `lr`.
          if (open_l > close_r)
            ( open_l - close_r + open_r, close_l )      // left result is the master.
          else
            ( open_r, close_r - open_l + close_l )      // right result is the master.
        }
    }

    // (`open`, `close`) of (0,0) means balanced parentheses.  Anything else is unbalanced.
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
