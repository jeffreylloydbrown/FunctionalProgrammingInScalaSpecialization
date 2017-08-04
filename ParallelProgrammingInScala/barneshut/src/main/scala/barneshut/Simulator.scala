package barneshut

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import scala.collection.parallel.TaskSupport
import scala.collection.parallel.Combiner
import scala.collection.parallel.mutable.ParHashSet
import common._

class Simulator(val taskSupport: TaskSupport, val timeStats: TimeStatistics) {

  /** The Barnes-Hut Algorithm is performed by iterating the same set of operations over and over.
    * This method encapsulates those operations.  Each operation is performed with multiple threads
    * processing the data.
    *
    * @param bodies   The sequence of `Body` objects that should interact with each other.
    * @return         A Pair that contains a (possibly smaller) sequence of `Body` objects for
    *                 the next call to `step()` along with the QuadTree that describes the
    *                 scene.  I wonder what the QuadTree is used for in `step()`'s caller....
    */
  def step(bodies: Seq[Body]): (Seq[Body], Quad) = {
    // 1. compute boundaries
    val boundaries = computeBoundaries(bodies)

    // 2. compute sector matrix
    val sectorMatrix = computeSectorMatrix(bodies, boundaries)

    // 3. compute quad tree
    val quad = computeQuad(sectorMatrix)

    // 4. eliminate outliers
    val filteredBodies = eliminateOutliers(bodies, sectorMatrix, quad)

    // 5. update body velocities and positions
    val newBodies = updateBodies(filteredBodies, quad)

    (newBodies, quad)
  }

  /** Updates the scene boundary values in `boundaries` to include `body` in the scene.
    *
    * @param boundaries   The current `Boundaries` object.  Updated in place, no new object created.
    * @param body         The `Body` to make sure is within the scene.
    * @return             `boundaries` with possibly new `minX`, `minY`, `maxX` and/or `maxY` values.
    */
  /*private*/ def updateBoundaries(boundaries: Boundaries, body: Body): Boundaries = {
    boundaries.minX = math.min(boundaries.minX, body.x)
    boundaries.minY = math.min(boundaries.minY, body.y)
    boundaries.maxX = math.max(boundaries.maxX, body.x)
    boundaries.maxY = math.max(boundaries.maxY, body.y)
    boundaries
  }

  /** Returns a new `Boundaries` object that covers the scene described by `a` and `b`, as well as the
    * space between them (if any).  `a` and `b` are no longer valid after this method runs.
    *
    * @param a    A `Boundaries` object representing a scene.
    * @param b    A `Boundaries` object representing a scene, possibly the same scene as `a`.
    * @return     A new `Boundaries` object that covers `a`, `b`, and any space between them.  `a` and
    *             `b` are no longer valid after this method runs.
    */
  /*private*/ def mergeBoundaries(a: Boundaries, b: Boundaries): Boundaries = {
    val result: Boundaries = new Boundaries()
    result.minX = math.min(a.minX, b.minX)
    result.minY = math.min(a.minY, b.minY)
    result.maxX = math.max(a.maxX, b.maxX)
    result.maxY = math.max(a.maxY, b.maxY)
    result
  }

  /** Step 1: Determine the smallest scene that contains all the `bodies`.  We do this in parallel.
    *
    * @param bodies   The sequence of `Body` objects in this simulation step.
    * @return         The `Boundaries` object that contains all the `bodies`.
    */
  def computeBoundaries(bodies: Seq[Body]): Boundaries = timeStats.timed("boundaries") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.aggregate(new Boundaries)(updateBoundaries, mergeBoundaries)
  }

  /** The homework says to follow the model of `computeBoundaries()`, which receives a sequence of bodies
    * but calls `updateBoundaries()` on a single body.  I'm doing `{_ += _}` as an anonymous function
    * in `computeSectorMatrix()`.  I don't use this method, it's here for documentation of that anonymous
    * function.
    *
    * @param sm     The `SectorMatrix` to which `body` is added.
    * @param body   The `Body` to add to `sm`
    * @return       `sm` is returned, updated with `body` in it.
    */
  /*private*/ def updateSectorMatrix(sm: SectorMatrix, body: Body): SectorMatrix = sm += body

  /** Again following the pattern:  `mergeSectorMatrices()` allocates a new object and stuffs the combination
    * into it.  But since the `combine()` operation does that in the `ConcBuffer`, I don't have to make a
    * new object.  I'm no longer using this method, it's here for documentation only.
    *
    * @param a    One of the `SectorMatrix` inputs.
    * @param b    The other input.
    * @return     A new object that holds the combined results of `a` and `b`.  Both `a` and `b` are
    *             no longer valid after calling this method.
    */
  /*private*/ def mergeSectorMatrices(a: SectorMatrix, b: SectorMatrix): SectorMatrix = a combine b

  /** Step 2: Construct the `SectorMatrix` that covers the scene `boundaries` from Step 1 describes.
    * We do this in parallel as well.
    *
    * @param bodies       The sequence of `Body` objects in this simulation step.
    * @param boundaries   The `Boundaries` object from Step 1 that contains all the `bodies`.
    * @return             The final `SectorMatrix` combiner that covers the scene.
    */
  def computeSectorMatrix(bodies: Seq[Body], boundaries: Boundaries): SectorMatrix = timeStats.timed("matrix") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    //parBodies.aggregate(new SectorMatrix(boundaries, SECTOR_PRECISION))(updateSectorMatrix, mergeSectorMatrices)
    parBodies.aggregate(new SectorMatrix(boundaries, SECTOR_PRECISION))( {_ += _}, {_ combine _} )
  }

  /** Step 3:  Convert the `sectorMatrix` (from Step 2) into a QuadTree.  Also done in parallel (in `toQuad()`).
    *
    * @param sectorMatrix   The `SectorMatrix` from Step 2 that describes the sequence of `bodies` in the scene.
    * @return               The QuadTree that describes the sequence of `bodies` in the scene.
    */
  def computeQuad(sectorMatrix: SectorMatrix): Quad = timeStats.timed("quad") {
    sectorMatrix.toQuad(taskSupport.parallelismLevel)
  }

  /** Step 4:  During the execution of Barnes-Hut, some of the bodies tend to move far away from most
    * of the other bodies.  There are many ways to deal with such outliers, but to keep things simple we
    * will eliminate bodies that move too fast and too far away.
    *
    * @param bodies         The sequence of `Body` objects we had in Step 1.
    * @param sectorMatrix   The `SectorMatrix` from Step 2 that describes those `bodies`.
    * @param quad           The QuadTree from Step 3 that describes those `bodies`.
    * @return               A new sequence of `Body` objects, possibly fewer in number than in Step 1.
    *                       Any bodies too far away that are moving too fast got thrown away.
    */
  def eliminateOutliers(bodies: Seq[Body], sectorMatrix: SectorMatrix, quad: Quad): Seq[Body] = timeStats.timed("eliminate") {

    /** A Body `b` is an outlier if it is far enough away, and it is moving away from `quad`'s center of
      * mass faster than twice the escape velocity.  Otherwise it is not an outlier.
      *
      * "far enough away" is controlled by `eliminationThreshold`, and defaults to 1/2 the size of the scene.
      *
      * @param b    The Body being examined as a possible outlier.
      * @return     `true` if `b` is an outlier and `false` otherwise.
      */
    def isOutlier(b: Body): Boolean = {
      val dx = quad.massX - b.x
      val dy = quad.massY - b.y
      val d = math.sqrt(dx * dx + dy * dy)
      // object is far away from the center of the mass
      if (d > eliminationThreshold * sectorMatrix.boundaries.size) {
        val nx = dx / d
        val ny = dy / d
        val relativeSpeed = b.xspeed * nx + b.yspeed * ny
        // object is moving away from the center of the mass
        if (relativeSpeed < 0) {
          val escapeSpeed = math.sqrt(2 * gee * quad.mass / d)
          // object has the escape velocity if its speed > twice the escape speed.
          -relativeSpeed > 2 * escapeSpeed
        } else false
      } else false
    }

    /** I need to read about what's happening here....  I'm only guessing.  Based on how it is used
      * below, determine the set of outliers in sector (`x`, `y`).
      *
      * @param x    The X axis sector coordinate under consideration.
      * @param y    The Y axis sector coordinate under consideration.
      * @return     A `Combiner` representing the set of outliers in sector (`x`, `y`).
      */
    def outliersInSector(x: Int, y: Int): Combiner[Body, ParHashSet[Body]] = {
      val combiner = ParHashSet.newCombiner[Body]
      combiner ++= sectorMatrix(x, y).filter(isOutlier)
      combiner
    }

    // `borderSectors` is the set of sectors along the edge of the scene.  Running the for-comprehension
    // on `verticalBorder` between 1 and sectorPrecision-1 prevents the corners from overlapping (making the
    // combined result a set instead of a group).
    val sectorPrecision = sectorMatrix.sectorPrecision
    val horizontalBorder = for (x <- 0 until sectorPrecision; y <- Seq(0, sectorPrecision - 1)) yield (x, y)
    val verticalBorder = for (y <- 1 until sectorPrecision - 1; x <- Seq(0, sectorPrecision - 1)) yield (x, y)
    val borderSectors = horizontalBorder ++ verticalBorder

    // compute the set of outliers.  I wish Coursera would have put the type on `outliers`, I've no idea of the type.
    // Seq?  Vector?  Array?  Some kind of Buffer?  It is something with a fast `apply` method though, so it certainly
    // isn't a List.
    val parBorderSectors = borderSectors.par
    parBorderSectors.tasksupport = taskSupport
    val outliers = parBorderSectors.map({ case (x, y) => outliersInSector(x, y) }).reduce(_ combine _).result

    // filter the bodies that are outliers
    val parBodies = bodies.par
    parBodies.filter(!outliers(_)).seq
  }

  /** Step 5:  Use the QuadTree `quad` from Step 3 to update the filtered `bodies` from Step 4 with new
    * velocities and positions.  Replaces the filtered sequence of `Body` objects with new `Body` objects
    * that account for the effects of Step 3.  We perform this in parallel as well.
    *
    * @param bodies     The result from Step 4, so not necessarily the same bodies as Step 1 had.
    * @param quad       The QuadTree from Step 3 that describes the scene.
    * @return           The contents of `bodies` is replaced with updated `Body` objects for use in the
    *                   next iteration of the algorithm.
    */
  def updateBodies(bodies: Seq[Body], quad: Quad): Seq[Body] = timeStats.timed("update") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.map(_.updated(quad)).seq
  }

}
