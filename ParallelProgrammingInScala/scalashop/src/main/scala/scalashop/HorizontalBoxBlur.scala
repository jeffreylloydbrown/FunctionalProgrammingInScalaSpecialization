package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   *
   *  @param src     The image being blurred (where the pixel data comes from).
   *  @param dst     The image being updated (where the blurred `src` pixels get recorded).
   *  @param from    The starting position of the row, inclusive.
   *  @param end     The ending position of the row, exclusive.  `from` <= `y` < `end`
   *  @param radius  The number of pixels left, right, up and down to average over in order to blur pixel(x,y).
   *  @return        Nothing.  `dst` is modified as a side-effect of calling this routine.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (y: Int <- from until end;
         x: Int <- 0 until src.width)
    { dst(x,y) = boxBlurKernel(src, x, y, radius) }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   *
   *  @param src       The image being blurred (where the pixel data comes from).
   *  @param dst       The image being updated (where the blurred `src` pixels get recorded).
   *  @param numTasks  Divide `src` into `numTasks` rows and process them in parallel.
   *                   1 <= `numTasks` required.  If `numTasks` is too large, only use up to a task per row.
   *  @param radius    The number of pixels left, right, up and down to average over in order to blur pixel(x,y).
   *                   `radius` >= 0 required.
   *  @return          Nothing.  `dst` is modified as a side-effect of calling this routine.
   *  @throws          `java.lang.IllegalArgumentException` if required values above not present.
   */
  @throws[java.lang.IllegalArgumentException]
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // Check parameters.
    require(numTasks >= 1, s"parBlur: parameter `numTasks` value `$numTasks` must be >= 1")
    require(radius >= 0, "parBlur: radius cannot be negative")

    // Make sure we don't have more tasks than rows, and determine the size for most strips.
    val num_tasks: Int = clamp(numTasks, 1, src.height)    // don't throw if requesting too many tasks.
    val strip_size: Int = src.height / num_tasks

    // Spawn `num_tasks` tasks.  Since we put the possible left overs into the first strip in `strip_bounds`,
    // it might have a little more work to do.  Since the order we process the strips doesn't matter, we start it
    // first (instead of last) so that it has a better chance of being done before the other strips finish.
    val tasks = for ( (from, end) <- stripBoundaries(strip_size, (num_tasks-1)*strip_size, src.height) )
      yield task { blur(src, dst, from, end, radius) }

    // I actually saw test failures in the 1 thread test when I didn't do the join and started removing debug messages.
    // I started with tasks.map(_.join), but got a warning about using a method returning Unit.  Yeah, ok.
    for (t <- tasks) t.join
  }

}
