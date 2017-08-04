package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

/** The K-Means algorithm for cluster detection identifies `k` clusters around which `n` points cluster.
  * Given a set of 3D Points `points`, the algorithm is:
  *
  * 1. Pick `k` points called `means`.  This is called initialization.
  * 2. Associate each value in `points` with the mean that is closest to it.  We obtain `k` clusters of
  *    points, and we refer to this process as classifying the points.
  * 3. Update each mean to have the average value of the corresponding cluster.
  * 4. If the `k` means have changed significantly (that is, if the change is larger than some threshold `eta`),
  *    go back to step 2.  If they did not, we say that the algorithm converged.
  * 5. The `k` means represent different clusters---every point is in the cluster corresponding to the closest mean.
  *
  * Making great use of this algorithm requires picking the initial `means` "smartly" and selecting an appropriate
  * value of `eta`.  Both "smartly" and "appropriate" are vague on purpose....
  */
class KMeans {

  /** Builds a random sequence of points.  Used for testing.
    *
    * @param k      The number of cluster points (e.g. `means`) we'll use.
    * @param num    The number of points to generate.
    * @return       A sequence of 3D points.
    */
  /*private*/
  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  /** Given a sequence of Points (for instance, from `generatePoints()` ) and the same number of desired means
    * used to generate those points, returns an initial guess at the 3D points around which `points` cluster.
    * These guesses all come from `points`.
    *
    * @param k        The desired number of clustering points, i.e. means.
    * @param points   The sequence of 3D Points from which to select guesses.
    * @return         A sequence of `k` Points that represent our initial clustering guesses.
    */
  /*private*/
  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  /** Given a sequences of `means` and a specific Point `p`, return the specific mean that is "closest" to `p`.
    * "closest" is the minimum 3D distance between the `point` and each candidate `mean`.
    *
    * @param p      The specific point in space to which we compare each mean.
    * @param means  The sequence of mean points, each of which is compared to `p`.
    * @throws IllegalArgumentException  if `means` is empty
    * @return       The point in `means` that has the minimum distance to `p`.  It is not the index into `means`,
    *               it is the specific Point value.
    */
  @throws[IllegalArgumentException]
  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  /** Given a list of `points` compute the 3D mean of those points.  If `points` is empty, returns `oldMean`.
    *
    * (Should have returned an Option(Point) instead of passing `oldMean`. Oh well.)
    *
    * @param oldMean  If `points` is empty, `oldMean` is returned because we must return something.
    * @param points   The sequence of Points for which to calculate the average point.
    * @return         The 3D Point that averages out all `points` on an x/y/z basis.
    */
  private def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.length == 0) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  /** `classify()` implements Step 2 of the K-Means Algorithm.  Given a sequence of 3D `points` and a sequence
    * of `means`, classify each point by which mean it is closest to.
    *
    * @param points   Sequence of 3D points we are classifying.
    * @param means    Sequence of mean value points we are to classify by.
    * @throws IllegalArgumentException  if `points` is not empty and `means` is empty.
    * @return         A map from a mean to a sequence of all the points clustered around that mean.
    */
  @throws[IllegalArgumentException]
  /*private*/
  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    if (points.nonEmpty && means.isEmpty)
      throw new IllegalArgumentException("classify(): means cannot be empty if points is not empty")
    else if (points.isEmpty && means.nonEmpty)
      // Map every mean to a sequence of no points.  Makes ( mean1 -> (), mean2 -> (), mean3 -> (), ... )
      // It always returns a sequential map which is fine since there are no points to process.
      means.map{ m => m -> GenSeq[Point]() }.toMap
    else
      // points and means both empty --> returns an empty Map (correct).  So no separate check needed.
      points                                         // GenSeq(Point(1,2,3), Point(4,5,6))
        // Convert the sequence of points into a sequence of pairs of ( closest mean, point ).
          .map{ p => ( findClosest(p, means), p ) }  // ( (mean1, Point(1,2,3)), (mean2, Point(4,5,6))
        // Group by the closest mean, which is the first element in each pair
          .groupBy(_._1)             // ( mean1 -> ( (mean1, Point(1,2,3)), ...), mean2 -> (mean2, Point(4,5,6)), ... )
        // For every value pair, replace it with the second pair element which is the cluster of Points
          .mapValues(_.map(_._2))                    // ( mean1 -> (Point(1,2,3), ...), mean2 -> (Point(4,5,6), ...) )
  }

  /** `update()` implements Step 3 of the K-Means Algorithm.  For each cluster, it computes the average point,
    * the new `mean` of that cluster (since clusters can change on each loop thru the algorithm).
    *
    * @param classified   The map of classified points from Step 2 (e.g. from `classify()`)
    * @param oldMeans     The sequence of means before the update.  Use this to find each cluster in `classified`.
    * @return             The calculated new sequence of means that gets fed into Step 4.
    */
  /*private*/
  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] =
    oldMeans.map{ mean: Point => findAverage(mean, classified(mean)) }

  /** `converged()` implements Step 4 of the K-Means Algorithm.  It pairs up each point in `oldMeans` and
    * `newMeans` and returns `true` iff the distance between each point in the pair is <= `eta`.
    *
    * @param eta        The convergence threshold: we are converged if each point in `oldMeans` is <= `eta`
    *                   away from the corresponding point in `newMeans`.
    * @param oldMeans   One set of means.
    * @param newMeans   The other set of means.  `oldMeans.length` must equal `newMeans.length`.
    * @throws IllegalArgumentException   if `oldMeans.length != newMeans.length`.
    * @return           True if the distance between `oldMeans(i)` and `newMeans(i)` <= `eta` for all points.
    *                   False otherwise.
    */
  @throws[IllegalArgumentException]
  /*private*/
  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    require(oldMeans.length == newMeans.length, "converged():  `oldMeans` and `newMeans` must be same size.")
    (oldMeans zip newMeans).forall{ pair: (Point, Point) => pair._1.squareDistance(pair._2) <= eta }
  }

  /** kMeans() implements Steps 2-4 of the K-Means Algorithm.  It must be handed an initial set of `means` (Step 1)
    * and returns the final set of clustering points (Step 5).
    *
    * @param points   Sequence of 3D points for which we calculate `k` cluster points.
    * @param means    The current guess at clustering points.  `k` == `means.length`.
    * @param eta      The convergence threshold, where recursion stops if distance between each current
    *                 mean and each new mean is <= `eta`.
    * @throws IllegalArgumentException if `eta` is not >= 0.
    * @return         The final set of Points around which `points` cluster.
    */
  @throws[IllegalArgumentException]
  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    require(!(points.nonEmpty && means.isEmpty), "kMeans(): means cannot be empty if points is not empty")
    require(eta >= 0, "kMeans():  eta is `$eta`, must be >= 0")
    lazy val new_means = update(classify(points, means), means)
    if (converged(eta)(means, new_means)) new_means
    else kMeans(points, new_means, eta)
  }
}


/** Describes one point in three-dimensional space.
  *
  *  Note: deliberately uses reference equality.
  *
  * @param x   The x axis coordinate in the 3D Cartesian volume.
  * @param y   The y axis coordinate in the 3D Cartesian volume.
  * @param z   The z axis coordinate in the 3D Cartesian volume.
  */
class Point(val x: Double, val y: Double, val z: Double) {
  /** Returns the square of its argument. */
  private def square(v: Double): Double = v * v

  /** Calculates the square of the 3D distance between `this` point and another point, `that`.
    * The distance is left squared since we really don't need to add the cost of a square root operation
    * in order to solve the algorithm.
    *
    * @param that   The Point for which we want to know the (squared) distance from us.
    * @return       The square of the distance to `that`.
    */
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  /** Rounds its Double argument to 2 decimal places.  Warning:  values > Int.MaxValue/100 will overflow. */
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  /** Have Points print as (x, y, z) rounded to 2 decimal places. */
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
