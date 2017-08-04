
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer, containing 4 8-bit color channels. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Construct a list of strip boundaries for processing in parallel.
    *
    * We create a list of pairs, with each pair being (`from`, `end`) for the strip.  Because we're doing integer
    * math, there might be left overs to cover with the final strip.  We put that strip first in the list so
    * if it has a little extra work to finish then the processing for the other strips will cover that extra time.
    * Using a loop to update `from` and `end` each time makes that harder to do.
    *
    * @param stripSize            The distance between `from` and `end` in each boundary pair.
    * @param lastStripStart       The `from` coordinate where the final strip starts
    * @param lastStripEnd         The `end` coordinate where the final strip ends, usually either the image source
    *                             width (for vertical strips) or image source height (for horizontal strips).
    * @return                     A List of Pairs of `from`, `end` coordinates suitable for feeding into a for-
    *                             comprehension.  The "final" strip is first in the list since it might (but not
    *                             always) have additional work to complete by being larger than the rest of the strips.
    **/
  def stripBoundaries(stripSize: Int, lastStripStart: Int, lastStripEnd: Int): List[(Int, Int)] = {
    (lastStripStart, lastStripEnd) ::
      ((0 until lastStripStart by stripSize) zip (stripSize until lastStripEnd by stripSize)).toList
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image.
    *  @param src     The image being blurred (where the pixel data comes from).
    *  @param x       The horizontal position of the pixel, starting at zero and increasing left to right.
    *  @param y       The vertical position of the pixel, starting at zero and increasing down the image.
    *  @param radius  The number of pixels left, right, up and down to average over in order to blur pixel(x,y).
    *  @return        The blurred pixel value for a new image.  `src` is not changed,
    *                 `boxBlurKernel()` caller must put the return value where it belongs.
    *
    *  According to the assignment, blurring includes all pixels in the averaging, and that means the target pixel
    *  as well.  We're to do it with nested while loops instead of using higher order functions---I considered
    *  building a List of the pixel data for each channel and decided to just use imperative programming in this case,
    *  since we're required to use while loops.
    **/
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // Check parameters are valid. `src` must exist since its constructor builds an empty array.  Don't check it.
    require(x == clamp(x, 0, src.width-1), s"boxBlurKernel: parameter `x` value `$x` outside of image")
    require(y == clamp(y, 0, src.height-1), s"boxBlurKernel: parameter `y` value `$y` outside of image")
    require(radius >= 0, "boxBlurKernel: radius cannot be negative")

    if (radius == 0) src(x, y)   // A small performance optimization: average of just yourself is yourself.
    else {
      var r: Int = 0 // sum of red channel for pixels in radius, not using `red` since that is a method name above.
      var g: Int = 0 // sum of green channel for pixels in radius
      var b: Int = 0 // sum of blue channel for pixels in radius
      var a: Int = 0 // sum of alpha channel for pixels in radius

      // These are the valid coordinate boundaries over which we'll loop, below.  Calling clamp() makes sure we
      // don't go outside the image horizontal and vertical boundaries.
      val x_start: Int = clamp(x - radius, 0, src.width-1)
      val y_start: Int = clamp(y - radius, 0, src.height-1)
      val x_end: Int = clamp(x + radius, 0, src.width-1)
      val y_end: Int = clamp(y + radius, 0, src.height-1)

      // Loop over the appropriate pixels.  `xx` and `yy` are the loop counters.  For each
      // pixel, access each of the 4 channels and update the appropriate accumulators.
      var xx: Int = x_start
      while (xx <= x_end) {

        var yy: Int = y_start
        while (yy <= y_end) {
          r += red(src(xx, yy))
          g += green(src(xx, yy))
          b += blue(src(xx, yy))
          a += alpha(src(xx, yy))

          yy += 1
        }

        xx += 1
      }

      // `r`, `g`, `b`, `a` each hold the pixel sums we need to average.  `pixels` is the number of
      // pixels we processed.  Could count it, but easy to calculate it 1 time instead of N*N additions of 1.
      val pixels: Int = (x_end - x_start + 1) * (y_end - y_start + 1)
      rgba(r / pixels, g / pixels, b / pixels, a / pixels)
    }
  }
}
