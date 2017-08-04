import java.lang.IllegalArgumentException

import common._
import barneshut.conctrees._

package object barneshut {

  /** `Boundaries` determine the size of the scene into which all bodies fit.
    * It starts as a very large 2D Cartesian plane, with X increasing left to right and Y
    * increasing top to bottom.
    *
    * NOTE:  We initialize backwards so we can always use `math.min()` to update the `boundaries.min`
    * values and `math.max` to update the `boundaries.max` values.
    */
  class Boundaries {
    /** The X coordinate of the left side of the scene. */
    var minX = Float.MaxValue
    /** The Y coordinate of the top side of the scene. */
    var minY = Float.MaxValue
    /** The X coordinate of the right side of the scene. */
    var maxX = Float.MinValue
    /** The Y coordinate of the bottom side of the scene. */
    var maxY = Float.MinValue
    /** The length of the top and bottom edges of the scene. */
    def width = maxX - minX
    /** The length of the left and right edges of the scene. */
    def height = maxY - minY
    /** The length of a side of the scene, the maximum of `width` and `height`.  Which begs the
      * question:  "what actually enforces the scene's squareness?"  "What happens when `width`
      * or `height` doesn't equal `size`???
      */
    def size = math.max(width, height)
    /** The X coordinate of the center of the scene in 2D space. */
    def centerX = minX + width / 2
    /** The Y coordinate of the center of the scene in 2D space. */
    def centerY = minY + height / 2
    /** Prints `Boundaries` by including their `minX`, `minY`, `maxX`, and `maxY` coordinates. */
    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  /** A QuadTree is used to divide the 2D cartesian plane into square cells, one for each quadrant.
    * (Hence the name.)  This allows the simulation to approximate mass at a distance as 1 big `Body`
    * with the mass of all the actual bodies in the cluster, positioned at the center of mass of the
    * cluster.
    *
    * The cell origin (0,0) is the top left corner.  The X axis increases to the right and the Y axis
    * increases downward.  Negative X and Y coordinates make no sense in a cell.
    *
    * QuadTrees are made from 3 different kinds of data types:  `Empty`, `Leaf`, and `Fork`.
    */
  sealed abstract class Quad {
    /** The X coordinate of the center of mass of the bodies in the represented cell. */
    def massX: Float
    /** The Y coordinate of the center of mass of the bodies in the represented cell. */
    def massY: Float
    /** The total mass of the bodies in the represented cell. */
    def mass: Float
    /** The X coordinate of the center of the represented cell in 2D space. */
    def centerX: Float
    /** The Y coordinate of the center of the represented cell in 2D space. */
    def centerY: Float
    /** The length of a side of the represented cell.  The cell is square. */
    def size: Float
    /** The number of bodies contained in the represented cell. */
    def total: Int

    /** Add Body `b` to the represented cell without changing the area in space the cell covers.
      *
      * @param b    The Body to add to the represented cell.
      * @return     A new QuadTree that contains what the old QuadTree contained plus `b`.
      */
    def insert(b: Body): Quad
  }

  /** `Empty` is an empty QuadTree.  It has no mass, no bodies, and the center of mass is the cell's
    * center in space.
    *
    * @param centerX    The X coordinate of the cell's center in space.
    * @param centerY    The Y coordinate of the cell's center in space.
    * @param size       The length of each side of the cell.
    */
  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    /** The X coordinate of the center of mass is always the X coordinate of the center of the cell. */
    def massX: Float = centerX
    /** The Y coordinate of the center of mass is always the Y coordinate of the center of the cell. */
    def massY: Float = centerY
    /** The total mass of an `Empty` is always 0. */
    def mass: Float = 0f
    /** There are no bodies in an `Empty`. */
    def total: Int = 0

    /** Add `Body` `b` to an `Empty` and you get a `Leaf` back.
      *
      * @param b    The `Body` to add to the represented cell.
      * @return     A new QuadTree that contains just `b`.
      */
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  /** `Fork` represents 4 quadrants:  `nw`, `ne`, `sw` and `se`, each of which is itself a QuadTree.
    * If you divide the cell into 4 sub-cells at the center of the cell, these 4 QuadTrees are the bodies
    * in those 4 sub-cells.
    *
    * The constructor assumes these 4 sub-cells are the same size and adjacent to each other, and laid out
    * as described cover the space of this cell.
    *
    * @param nw    The QuadTree in the top left of the cell.
    * @param ne    The QuadTree in the top right of the cell.
    * @param sw    The QuadTree in the bottom left of the cell.
    * @param se    The QuadTree in the bottom right of the cell.
    */
  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    // The sub-cells are supposed to be square, which means all their sizes must be equal.
    require(nw.size == ne.size && ne.size == sw.size && sw.size == se.size && se.size == nw.size,
            "Attempted to create a QuadTree out of sub-cells that aren't equal in size.")

    /** The X coordinate of the center is half-way between the nw and ne cells' center. */
    // val centerX: Float =(quads.map(_.centerX).min + quads.map(_.centerX).max) / 2f
    val centerX: Float = (nw.centerX + ne.centerX) / 2f
    /** The Y coordinate of the center is half-way between the nw and sw cells' center. */
    //val centerY: Float =(quads.map(_.centerY).min + quads.map(_.centerY).max) / 2f
    val centerY: Float = (nw.centerY + sw.centerY) / 2f
    /** The `size` of this cell is double the size of any sub-cell.  Above I enforce "squareness". */
    val size: Float = 2f * nw.size
    /** `mass` is the total mass of all the quads. */
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    /** The center of mass X coordinate is `centerX` if `mass` is zero.  Otherwise compute the formula
      * from the homework assignment:  (for each quad sum(mass * massX))/ mass
      * I could get cute and use a fold, but there are only 4 quads and writing it out is easier to follow.
      */
    val massX: Float = {
      if (mass == 0.0) centerX
      else (nw.mass*nw.massX + ne.mass*ne.massX + sw.mass*sw.massX + se.mass*se.massX)/mass
    }
    /** The center of mass Y coordinate is `centerY` if `mass` is zero.  Otherwise compute the formula
      * from the homework assignment:  (for each quad sum(mass * massY))/ mass
      * I could get cute and use a fold, but there are only 4 quads and writing it out is easier to follow.
      */
    val massY: Float = {
      if (mass == 0.0) centerY
      else (nw.mass*nw.massY + ne.mass*ne.massY + sw.mass*sw.massY + se.mass*se.massY)/mass
    }
    /** The overall `total` is just the totals of each sub-cell. */
    val total: Int = nw.total + ne.total + sw.total + se.total

    /** Add Body `b` into the bodies we already have.  We have to figure out which sub-cell `b` will be in, then
      * create a new `Fork` with `b` inserted into that sub-cell.
      *
      * `nw` occupies left of `centerX` and above `centerY`.
      * `ne` occupies right of `centerX` and above `centerY`.
      * `sw` occupies left of `centerX` and below `centerY`.
      * `se` occupies right of `centerX` and below `centerY`.
      *
      * @param b    The Body to add to the represented cell.
      * @return     A new QuadTree that contains what the old QuadTree contained plus `b`.
      */
    def insert(b: Body): Fork = {
      if (b.x < centerX && b.y < centerY)         Fork(nw.insert(b), ne, sw, se)
      else if (!(b.x < centerX) && b.y < centerY) Fork(nw, ne.insert(b), sw, se)
      else if (b.x < centerY && !(b.y < centerY)) Fork(nw, ne, sw.insert(b), se)
      else                                        Fork(nw, nw, sw, se.insert(b))
    }
  }

  /** A `Leaf` is used when the size of an edge of the cell is <= `minimumSize`.  Essentially this
    * stops the recursive build of the QuadTree.  Even in such a small space, a `Leaf` can hold
    * quite a few `bodies`, so don't assume a `Leaf` has just one `Body` in it.
    *
    * @param centerX    The X coordinate of the cell's center in space (not necessarily center of mass).
    * @param centerY    The Y coordinate of the cell's center in space (not necessarily center of mass).
    * @param size       The length of each side of this `Leaf` cell.
    * @param bodies     The sequence of `Body` objects covered by this cell.
    */
  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    /** `mass` is the sum of the masses in `bodies`.  `bodies.map(_.mass).sum` also works, but less efficiently. */
    val mass: Float  = bodies.foldLeft(0f){ (sum, b) => sum + b.mass }
    /** `massX` is the X coordinate of the center of mass of this Leaf.
      * Calculate it by summing each body's mass*X position, then divide the sum by `mass`.
      * `bodies.map(b => b.mass*b.x).sum / mass` also works. (but does 2 passes thru `bodies`)
      */
    val massX: Float = bodies.foldLeft(0f){ (sum, b) => sum + b.mass*b.x } / mass
    /** `massY` is the Y coordinate of the center of mass of this Leaf.
      * Calculate it by summing each body's mass*Y position, then divide the sum by `mass`.
      * `bodies.map(b => b.mass*b.y).sum / mass` also works (but does 2 passes thru `bodies`)
      */
    val massY: Float = bodies.foldLeft(0f){ (sum, b) => sum + b.mass*b.y } / mass
    /** `total` is just the number of Body objects we have. */
    val total: Int = bodies.length

    /** Add `b` to our existing `bodies`.  If the size of a `Leaf` is greater than a predefined
      * `minimumSize`, inserting an additional body into that `Leaf` QuadTree creates a `Fork` QuadTree
      * with empty children, and adds all the `bodies` into that `Fork` (including the new body `b`).
      * Otherwise, inserting creates another `Leaf` with all the existing `bodies` and the new one.
      *
      * The tricky part is making a `Leaf` vs. making a `Fork`.  To make the `Fork` we have to make
      * each of the sub-cells then spread `b` + `bodies` amongst them.
      *
      * @param b    The Body to add to the represented cell.
      * @return     A new QuadTree that contains what the old QuadTree contained plus `b`.
      */
    def insert(b: Body): Quad = {
      val new_bodies = bodies :+ b
      if (size > minimumSize) {
        // Figure out where the centers of the 4 sub-cells will be.  Not the edges, the center
        // coordinates of the 4 sub-cells in space.  Remember that `size` spans 2 sub-cells,
        // so half_size is 1 sub-cell and quarter_size is the offset of the center in that 1 sub-cell.
        val half_size = size / 2f
        val quarter_size = size / 4f
        val leftX: Float = centerX - quarter_size
        val rightX: Float = centerX + quarter_size
        val topY: Float = centerY - quarter_size
        val bottomY: Float = centerY + quarter_size

        // nw centered at (leftX, topY); ne centered at (rightX, topY);
        // sw centered at (leftX, bottomY); se centered at (rightX, bottomY).

        // Make a new `Fork` that's all empty.  Then insert each of the
        // bodies in turn, including b, into the new `Fork` and return it.
        val fork: Fork = Fork(Empty(leftX, topY, half_size),    // nw
                              Empty(rightX, topY, half_size),   // ne
                              Empty(leftX, bottomY, half_size), // sw
                              Empty(rightX, bottomY, half_size) // se
                             )
        new_bodies.foldLeft(fork){ (f, body) => f.insert(body) }
      }
      else Leaf(centerX, centerY, size, new_bodies)
    }

  }

  /** Specifies the cell edge length that decides between making a `Leaf` and a `Fork`.  A `Leaf` has
    * an edge length no bigger than `minimumSize`.  (This is the recursion threshold value.)
    */
  def minimumSize = 0.00001f

  /** The gravitational constant `G`.  Not sure what measurement system though;
    * I learned 32 ft/sec^2^ or 9.8 m/sec^2^ for Earth, but those are little `g`.  Maybe it is a
    * rounding of 98 dm/sec^2^?  But who talks in decimeters???
    */
  def gee: Float = 100.0f

  /** The simulated change in time (in seconds) for a single algorithm iteration. */
  def deltaT: Float = 0.01f

  /** The threshold for deciding if a QuadTree is "far enough away to approximate with a single point".
    * Specifically, if `quad.size / distance to quad center of mass < theta`, the QuadTree is far enough
    * to approximate with a single point.
    */
  def theta = 0.5f

  /** Defines what "far enough away" means for eliminating bodies moving fast enough to leave the scene
    * and never make it back.  `eliminationThreshold` is applied to the size of the current scene, and a
    * body farther away than `eliminationThreshold * boundaries.size` is a candidate for eliminating.
    */
  def eliminationThreshold = 0.5f

  /** Returns the force of attraction between two masses `m1` and `m2` at distance `r` between them.  Unlike
    * in real physics, we are not negating the value to show attraction.  So this is the magnitude of the
    * force vector between the 2 objects.
    */
  def force(m1: Float, m2: Float, r: Float): Float = gee * m1 * m2 / (r * r)

  /** Returns the `distance` between 2 Cartesian points `(x0, y0)` and `(x1, y1)`. */
  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  /** `Body` represents an object with mass in our 2D Cartesian plane.
    *
    * @param mass     The mass of the object.
    * @param x        The X coordinate of this object in 2D space.
    * @param y        The Y coordinate of this object in 2D space.
    * @param xspeed   How fast the X coordinate is changing.
    * @param yspeed   How fast the Y coordinate is changing.
    */
  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    /** Returns `true` if `quad` is far enough away from this body to approximate the affect of
      * `quad` with a single point at `(quad.massX, quad.massY)` of mass `quad.mass`.
      *
      * @param quad   The QuadTree being compared to this body's position.
      * @return       `true` if it is OK to approximate `quad` and `false` if not.
      */
    def isFarEnoughAway(quad: Quad): Boolean = quad.size/distance(x, y, quad.massX, quad.massY) < theta

    /** Apply the QuadTree `quad`, which represents the bodies that can act gravitationally on this body,
      * to this body and return an updated version of the body.
      *
      * @param quad   The QuadTree describing this Body's surroundings.
      * @return       A new Body that applies the forces from the surroundings.
      */
    def updated(quad: Quad): Body = {
      // Accumulates the magnitude of the force vector applied on this body by `quad`.
      // `|force|` = `(netforcex, netforcey)`.
      var netforcex = 0.0f
      var netforcey = 0.0f

      /** If the distance is smaller than 1f, we enter the realm of close
        * body interactions. Since we do not model them in this simplistic
        * implementation, bodies at extreme proximities get a huge acceleration,
        * and are catapulted from each other's gravitational pull at extreme
        * velocities (something like this:
        * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
        * To decrease the effect of this gravitational slingshot, as a very
        * simple approximation, we ignore gravity at extreme proximities.
        */
      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)

        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      /** Examine `quad` and apply the forces each `body` in it represents to this body. */
      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force so no change in position.
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach{ b: Body => addForce(b.mass, b.x, b.y) }
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body, or recursion is needed on each sub-cell.
          if (isFarEnoughAway(quad)) addForce(quad.mass, quad.massX, quad.massY)
          else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      // Remember:
      // position `n` = `n0` + `v`*`change in time`.  `n` = `(nx, ny)`
      // velocity `v` = `v0` + `a`*`change in time`.  `v` = `(nxspeed, nyspeed)`
      // acceleration `a` = `net change in force`/`mass` .  `netforce` = `(netforcex, netforcey)`
      val nx = x + xspeed * deltaT
      val ny = y + yspeed * deltaT
      val nxspeed = xspeed + netforcex / mass * deltaT
      val nyspeed = yspeed + netforcey / mass * deltaT

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  /** A convenient "default" for the number of rows and the number of columns in `SectorMatrix`.  The
    * actual value used by `SectorMatrix` is passed to it in its constructor.
    */
  val SECTOR_PRECISION = 8

  /** `SectorMatrix` is a square matrix that covers a square region of space specified by `boundaries`.
    * Each square is a `sector`, and each `sector` will be represented by its own QuadTree.  It acts as
    * a combiner by partitioning the square region of space into `sectorPrecision^2^` buckets.  This
    * makes it easy to parallelize the combining operation, since each data bucket is separate and doesn't
    * overlap.  A `Body` will belong to at most a single `sector`.
    *
    * @param boundaries       The `Boundaries` of the scene (e.g. the "universe").
    * @param sectorPrecision  The width and height of the matrix; there are `sectorPrecision^2^` squares
    *                         in the `SectorMatrix`.
    */
  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    // The size of the side of each `sector`, which will also be the size of the QuadTree cell
    // for each `sector`.
    val sectorSize = boundaries.size / sectorPrecision
    // `matrix` is a linear array instead of an actual matrix (2D array).  An `apply` method below
    // provides a translation between `x`, `y` coordinates and the index into `matrix`.
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    // Initialize each `sector` with a `ConcBuffer`.  We'll use that to build QuadTrees for each `sector`
    // and to combine QuadTrees to cover the entire scene in parallel.
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    /** Given a position `pos` and the `least` allowed position on that axis, return the sector index that
      * covers the position.  Snap any position < `least` to index 0, and any position outside the maximum
      * boundary to `sectorPrecision-1`.  We know we are outside the boundary if the distance in sectors is
      * bigger than `sectorPrecision-1`.
      *
      * @param pos      The position to convert to a sector index.
      * @param least    The lowest in-bounds position on this axis.
      * @return         The sector index that covers `pos`.  Index 0 covers any position left or above
      *                 the minimum boundaries.  Index `sectorPrecision-1` covers any position right or
      *                 below the maximum boundaries.
      */
    private def sectorCoordinate(pos: Float, least: Float): Int = {
      // Figure out the distance between `pos` and `least` in sectors, converted to an Int.
      val num_sectors: Int = ( (pos - least) / sectorSize ).toInt
      // If num_sectors < 0, snap to 0 (return the least row/col index).
      // If num_sectors > sectorPrecision-1, snap to sectorPrecision-1 (return the most row/col index).
      // Otherwise, return num_sectors.  This is just min(max(0, num_sectors), sectorPrecision-1)!
      // Simple and fast!
      math.min(math.max(0, num_sectors), sectorPrecision-1)
    }

    /** Figure out which `sector` should hold `Body` `b` and add it to the `ConcBuffer` representing that
      * sector.  The work is figuring out the correct `x` and `y` sector coordinates; we use the
      * `ConcBuffer`'s `+=` method to actually add it.  If `b` lies outside `boundaries`, we are to
      * consider it to be snapped to the closest boundary side to figure out into which sector it goes.
      *
      * @param b    The `Body` being added to our scene.
      * @return     The current `SectorMatrix` so we can chain operations.
      */
    def +=(b: Body): SectorMatrix = {
      // Figure out the sector coordinates `x` and `y`.  It is critical that the X line be all X variables,
      // and the Y line be all Y variables.
      val x: Int = sectorCoordinate(b.x, boundaries.minX)
      val y: Int = sectorCoordinate(b.y, boundaries.minY)

      // The adding is easy now, thanks to the `apply` method below....
      this(x, y) += b
      this
    }

    // Given `x` and `y` sector coordinates, return the `ConcBuffer` for that `sector`.
    // WARNING:  This method came with the homework and I haven't changed it.  It has NO ERROR CHECKING,
    // so bad coordinates can read outside `matrix` and cause Java exceptions.
    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    /** Given another `SectorMatrix` `that`, construct a new `SectorMatrix` which represents the union
      * of bodies from `this` and `that`.  Both `this` and `that` are invalidated in the combination process.
      *
      * NOTE: we are told we can safely assume that `combine()` will only be called on matrices of the same
      * dimensions, boundaries and sector precision.  I'm still checking it though.
      *
      * On completion, the # of bodies in the new `SectorMatrix` must equal the # of bodies in `this` and `that`.
      *
      * @param that   The other `SectorMatrix` we are combining with.
      * @throws IllegalArgumentException if `this` and `that` are not compatible (see NOTE above).
      * @return       A new `SectorMatrix` that contains all the bodies from `this` and `that`.
      */
    @throws[IllegalArgumentException]
    def combine(that: SectorMatrix): SectorMatrix = {
      // We're told we can assume `this` and `that` are compatible.  BS.  Confirm it.
      require(this.matrix.length == that.matrix.length,
        s"SectorMatrix.combine() expected matrix of size ${this.matrix.length}, received matrix of size ${that.matrix.length}")
      require(this.sectorPrecision == that.sectorPrecision,
        s"SectorMatrix.combine() expected sectorPrecision of ${this.sectorPrecision}, received ${that.sectorPrecision} instead.")
      require(this.boundaries.size == that.boundaries.size,
        s"SectorMatrix.combine() expected boundaries of size ${this.boundaries.size}, received ${that.boundaries.size} instead.")

      // Remember what we started with.
      val this_bodies: Int = this.matrix.map(_.size).sum
      val that_bodies: Int = that.matrix.map(_.size).sum

      // OK, get to work.  We need to loop over each `ConcBuffer` in `this` and combine it with `that`.
      // In this instance, this.matrix(i).combine(that.matrix(i)) is harder to read....
      for (i: Int <- 0 until matrix.length) { this.matrix(i) = this.matrix(i) combine that.matrix(i) }

      // Test the postcondition.
      val new_body_count: Int = this.matrix.map(_.size).sum
      val old_body_count: Int = this_bodies + that_bodies
      require(new_body_count == old_body_count,
        s"SectorMatrix.combine() postcondition expects ${old_body_count} bodies, but found ${new_body_count}.")

      // Return ourselves so we can chain.
      this
    }

    /** Converts the ConcBuffers we're using to a QuadTree that contains all the bodies added with `+=`.
      * This code was provided as part of the homework assignment.  Thank goodness.
      *
      * @param parallelism  Helps figure out how much parallelism we can expect, so we don't spawn more
      *                     tasks than the hardware can really run in parallel.
      * @return             The QuadTree that represents `this` `SectorMatrix`.
      */
    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
