package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def giveMeSomePoints(num: Int): (GenSeq[Point], GenSeq[Point]) = {
    ( IndexedSeq(new Point(1, 1, 0),   new Point(1, -1, 0),   new Point(-1, 1, 0),   new Point(-1, -1, 0)).take(num),
      IndexedSeq(new Point(.5, .5, 0), new Point(.5, -.5, 0), new Point(-.5, .5, 0), new Point(-.5, -.5, 0)).take(num)
      )
  }

  // CLASSIFY tests

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected: GenMap[Point, GenSeq[Point]] = GenMap()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq(new Point(1,1,1))
    val expected = GenMap( means(0) -> GenSeq() )
    checkClassify(points, means, expected)
  }

  test("'classify' throws an exception for 'points'  == GenSeq(Point(1,1,1)) and empty 'means'") {
    intercept[IllegalArgumentException] {
      val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
      val means: GenSeq[Point] = IndexedSeq()
      classify(points, means)
    }
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val (points, _) = giveMeSomePoints(4)
    val means: GenSeq[Point] = IndexedSeq(new Point(0,0,0))
    val expected = GenMap( means(0) -> points )
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val (points, _) = giveMeSomePoints(4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap( mean1 -> GenSeq(points(0), points(1)) , mean2 -> GenSeq(points(2), points(3)) )
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected: GenMap[Point,GenSeq[Point]] = GenMap()
    checkParClassify(points, means, expected)
  }

  // UPDATE tests

  test("'update' (sequential) works with 1 Point that is both `point` and `mean`") {
    val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
    val old_means: GenSeq[Point] = points
    val new_means: GenSeq[Point] = update(classify(points, old_means), old_means)
    assert(old_means.toString === new_means.toString)   // floating point comparisons annoy me....
  }

  test("'update' (data parallel) works with 1 Point that is both `point` and `mean`") {
    val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
    val old_means: GenSeq[Point] = points
    val new_means: GenSeq[Point] = update(classify(points.par, old_means.par).par, old_means.par)
    assert(old_means.par.toString === new_means.par.toString)
  }

  test("'update' (sequential) works with 1 Point as mean and no points") {
    val points: GenSeq[Point] = GenSeq()
    val old_means: GenSeq[Point] = GenSeq(new Point(1, 1, 1))
    val classified: GenMap[Point, GenSeq[Point]] = GenMap[Point, GenSeq[Point]]( old_means(0) -> points )
    val new_means: GenSeq[Point] = update(classified, old_means)
    assert(old_means.toString === new_means.toString)
  }

  test("'update' (data parallel) works with 1 Point as mean and no points") {
    val points: GenSeq[Point] = GenSeq()
    val old_means: GenSeq[Point] = GenSeq(new Point(1, 1, 1))
    val classified: GenMap[Point, GenSeq[Point]] = GenMap[Point, GenSeq[Point]]( old_means(0) -> points )
    val new_means: GenSeq[Point] = update(classified.par, old_means.par)
    assert(old_means.par.toString === new_means.par.toString)
  }

  test("'update' (sequential) works for 'classified' == Map(Point(0, 0, 0) -> GenSeq((1, 1, 1), ... (99, 99, 99)) " +
       " and 'means' == GenSeq((0,0,0))") {
    val points: GenSeq[Point] = (1 to 99).map{ i => new Point(i, i, i) }
    val old_means: GenSeq[Point] = GenSeq(new Point(0, 0, 0))
    val classified: GenMap[Point, GenSeq[Point]] = GenMap( old_means(0) -> points )
    val new_means: GenSeq[Point] = update(classified, old_means)
    assert(new_means.toString === GenSeq(new Point(50,50,50)).toString)
  }

  test("'update' (parallel) works for 'classified' == Map(Point(0, 0, 0) -> GenSeq((1, 1, 1), ... (99, 99, 99)) " +
    " and 'means' == GenSeq((0,0,0))") {
    val points: GenSeq[Point] = (1 to 99).map{ i => new Point(i, i, i) }
    val old_means: GenSeq[Point] = GenSeq(new Point(0, 0, 0))
    val classified: GenMap[Point, GenSeq[Point]] = GenMap( old_means(0) -> points )
    val new_means: GenSeq[Point] = update(classified.par, old_means.par)
    assert(new_means.par.toString === GenSeq(new Point(50,50,50)).par.toString)
  }

  // CONVERGED tests

  test("'converged' throws an exception if 'points' and 'means' are not the same size") {
    intercept[IllegalArgumentException] {
      val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
      val means: GenSeq[Point] = IndexedSeq()
      converged(0.01)(points, means)
    }
  }

  test("'converged' returns true on empty 'points' and 'means'") {
    val old_means: GenSeq[Point] = IndexedSeq()
    val new_means: GenSeq[Point] = IndexedSeq()
    assert(converged(0.01)(old_means, new_means) === true)
  }

  test("'converged' returns true when old means == new means, even for zero eta") {
    val means: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
    assert(converged(0.0)(means, means) === true)
  }

  test("'converged' returns true on short 'points' and 'means' with eta==100") {
    val (old_means, new_means) = giveMeSomePoints(400)
    assert(converged(100.0)(old_means, new_means) === true)
  }

  test("'converged' returns false on short 'points' and 'means' with eta==.0001") {
    val (old_means, new_means) = giveMeSomePoints(400)
    assert(converged(0.0001)(old_means, new_means) === false)
  }

  // KMEANS tests.

  test("'kMeans' throws an exception if eta < 0.0") {
    intercept[IllegalArgumentException] {
      val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
      val means: GenSeq[Point] = points
      kMeans(points, means, -1e-20)
    }
  }

  test("'kMeans' with no 'points' and no 'means' returns empty") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected: GenMap[Point, GenSeq[Point]] = GenMap()
    assert(kMeans(points, means, 0.1).isEmpty)
  }

  test("'kMeans' throws an exception if 'points' is not empty and 'means' is empty") {
    intercept[IllegalArgumentException] {
      val points: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
      val means: GenSeq[Point] = IndexedSeq()
      kMeans(points, means, 0.1)
    }
  }

  test("'kMeans' works if 'points' is empty and 'means' is not empty") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
    assert(kMeans(points, means, 0.1).toString === means.toString)
  }

  test("'kMeans' Coursera submission test....") {
    val points: GenSeq[Point] = GenSeq(new Point(0,0,1), new Point(0,0,-1), new Point(0,1,0), new Point(0,10,0))
    val means: GenSeq[Point] = GenSeq(new Point(0,-1,0), new Point(0,2,0))
    assert(kMeans(points, means, 12.25).toString == GenSeq(new Point(0,0,0), new Point(0,5.5,0)).toString)
  }

  test("cover the performance functions without running full performance test") {
    val numPoints = 500
    val k = 8
    val points = generatePoints(k, numPoints)
    val means = initializeMeans(k, points)
    kMeans(points, means, 0.01)
  }
}


  
