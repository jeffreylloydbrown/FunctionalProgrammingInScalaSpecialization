package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  /** maxTangent() returns the maximum tangent between what's been seen so far `max_so_far` and the current
    * tangent calculated as `rise`/`run`
    *
    * @param max_so_far   The running maximum seen processing earlier tangent values.
    * @param rise         The 'y' of the slope, i.e. the height at `i` which is `input(i)`
    * @param run          The 'x' of the slope, i.e. the position `i`
    * @return             The new running maximum tangent
    */
  def maxTangent(max_so_far: Float, rise: Float, run: Int): Float = Math.max(rise/run, max_so_far)

  /** lineOfSightRecursive() uses the terrain map represented as heights in `input` to calculate the line of sight
    * tangents across that terrain.  The resulting angle tangents are returned in `output`.  It solves this
    * problem recursively instead of with functional programming.
    *
    * (Functional programming is starting to rub off on me.  When I first read the homework
    * spec, my first thought was "I can solve it with a foldLeft run on a zip of the input array
    * and a range that represents the array positions."  Might still try that if I have time.
    * Then we learned about scanLeft in a class lecture and I knew I could do it.  See below.)
    *
    * @param input     array representing terrain height at each position `i`
    * @param output    the tangents representing the line of sight angles at each position `i`
    * @return          `output` is an out parameter so it is completely replaced.
    */
  def lineOfSightRecursive(input: Array[Float], output: Array[Float]): Unit = {

    def losAux (i: Int, until: Int): Unit = {
      if (i < until)
        {
          output(i) = maxTangent(output(i-1), input(i), i)  // current maximum is at output(i-1)
          losAux(i+1, until)
        }
    }

    input(0) = 0                  // shouldn't be necessary, but guarantees starting position is correct.
    output(0) = Float.MinValue    // This allows negative tangents to work in the future.
    losAux(1, input.length)
    output(0) = input(0)          // This corrects the "starting condition".
  }

  /** lineOfSight() uses the terrain map represented as heights in `input` to calculate the line of sight
    * tangents across that terrain.  The resulting angle tangents are returned in `output`.
    *
    * @param input     array representing terrain height at each array index `i`
    * @param output    the tangents representing the line of sight angles at each array index `i`
    * @return          `output` is an out parameter so it is completely replaced.
    */
  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    input.                          // for example, (0f, 1f, 8f, 9f)
      // remove input(0) since scanLeft() will add it below, we initialize with it.
      drop(1).                      // (1f, 8f, 9f)
      // convert input(i) into ( input(i), 1 ) to use scanLeft() array cursor trick
      map(x => (x, 1) ).            // ( (1f, 1), (8f, 1), (9f, 1) )

      // Use scanLeft() to construct the tangent results.  Each previous value in scanLeft's result is the
      // maximum tangent seen so far.  So the current pair gets the max of the tangent (which is
      // `cur`/(prev_i + 1, or `i`) ) and the previous value `max_so_far` as its first element, and prev_i+1 (or `i`)
      // as its second element (the array cursor trick).  Can't use `cur_i` because it isn't calculated yet,
      // so it is still 1.
      scanLeft( (input(0), 0) )     // ( (0f, 0), (1f, 1), (4f, 2), (4f, 3) )
        { case ( (max_so_far: Float, prev_i: Int), (cur: Float, _) ) =>
          ( maxTangent(max_so_far, cur, prev_i+1), prev_i+1 ) }.

      // convert each ( output(i), i ) pair back to just output(i)
      map(pair => pair._1).         // (0f, 1f, 4f, 4f)
      copyToArray(output)           // and write it into the output array.

    // That's a 1-line implementation, baby!  Just spread out for comments and readability.
    // Now, is it faster than lineOfSightRecursive() ???
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = Math.max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle tangent.
    *
    * Note:  `(for (i <- from until end) yield input(i)/i).max` solves this, but instructions said to use a while loop.
    *
    * @param input    The terrain data.  All of it, not just a subarray.
    * @param from     The starting 'x' position into `input`.
    * @param end      The ending position in `input`, exclusive.  `from` <= `x` < `end`
    * @return         The maximum angle tangent seen between `from` up to `end`.
    */
  def upsweepSequential(input: Array[Float], from: Int, end: Int): Float = {
    var tangent: Float = input(from)/from
    var i: Int = from + 1
    while (i < end) {
      tangent = maxTangent(tangent, input(i), i)
      i = i + 1
    }
    tangent
  }

  /** Traverses the part of the array starting at `from` going up to but not including `end`, and returns the
    * reduction tree for that part of the array.
    *
    *  The reduction tree is a `Leaf` if the length of the specified part of the array is smaller or equal to
    *  `threshold`, and a `Node` otherwise.  If the specified part of the array is longer than `threshold`, then
    *  the work is divided and done recursively in parallel.
    *
    * @param input      The terrain data.  All of it, not just a subarray.
    * @param from       The starting `x` position into `input`.
    * @param end        The ending position in `input`, exclusive.  `from` <= `x` < `end`
    * @param threshold  Picks between sequential and parallel operations.  `end` - `from` <= `threshold` runs
    *                   sequentially.  Otherwise split the segment roughly in half and run both in parallel.
    * @return           The intermediate tree of results representing each segment and its maximum angle tangent.
    */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if (end - from <= threshold)
      Leaf(from, end, upsweepSequential(input, from, end))
    else
      {
        val mid = from + (end - from) / 2
        val (left, right) = parallel(upsweep(input, from, mid, threshold),
                                     upsweep(input, mid, end, threshold))
        Node(left, right)
      }
  }

  /** Traverses the part of the `input` array starting at `from` going up to but not including `until`, and
    * computes the maximum angle tangent for each entry of the output array, given the `startingAngle`.
    *
    * @param input          The terrain data.  All of it, not just a subarray.
    * @param output         The line of sight angle tangents.  Values between `from` and `until` get updated.
    * @param startingAngle  The angle tangent from the initial position, becomes the maximum seen so far as processing
    *                       progresses.
    * @param from           The starting `x` position into `input`.
    * @param until          The ending position in `input`, exclusive.  `from` <= `x` < `until`
    * @return               `output(from)` to `output(until-1)` are overwritten by this routine.
    */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
    if (from < until) {
      output(from) = maxTangent(startingAngle, input(from), from)
      downsweepSequential(input, output, output(from), from + 1, until)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the reduction `tree` in parallel, and
    * then calls `downsweepSequential` to write the `output` angles.
    *
    * @param input          The terrain data.  All of it, not just a subarray.
    * @param output         The line of sight angle tangents.  Completely updated by this routine.
    * @param startingAngle  The angle tangent from the initial position, becomes the maximum seen so far as processing
    *                       progresses.
    * @param tree           The intermediate results tree created by `upsweep`.
    * @return               `output` is completely overwritten by this routine.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float, tree: Tree): Unit = tree match {
    case Leaf(from, end, _) =>  downsweepSequential(input, output, startingAngle, from, end)
    case Node(left, right)  =>  parallel(downsweep(input, output, startingAngle, left),
                                         downsweep(input, output, Math.max(startingAngle, left.maxPrevious), right))
  }

  /** Compute the line-of-sight in parallel.
    *
    * @param input        The terrain data.  All of it, not just a subarray.
    * @param output       The line of sight angle tangents.  Must be at least as large as `input`.
    * @param threshold    Picks between sequential and parallel operations.  Array segments <= `threshold` run
    *                     sequentially.  Otherwise split the segment roughly in half and run both in parallel.
    * @return             `output` is completely overwritten by this routine.
    */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    require(output.length >= input.length, "output array is too small to hold results")
    require(threshold > 0, s"threshold value `$threshold` is not > 0")
    input.length match {
      case 0 =>   ()                      // no input data, so do nothing.
      case 1 =>   output(0) = input(0)    // degenerate case, no need to do any processing.
      case _ =>
        output(0) = input(0)
        downsweep(input, output, input(0), upsweep(input, 1, input.length, threshold))
    }
  }
}
