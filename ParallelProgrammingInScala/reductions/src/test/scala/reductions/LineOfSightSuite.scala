package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    lineOfSight(input, output)
    assert(output.toList === List(0f, 1f, 4f, 4f))
    val outputRec = new Array[Float](4)
    lineOfSightRecursive(input, outputRec)
    assert(outputRec.toList === output.toList)
  }

  test("lineOfSight should correctly handle an array of size 1") {
    val output = new Array[Float](1)
    lineOfSight(Array[Float](0f), output)
    assert(output.toList === List(0f))
  }

  test("lineOfSight should correctly handle an array of size 2") {
    val output = new Array[Float](2)
    lineOfSight(Array[Float](0f, 43f), output)
    assert(output.toList === List(0f, 43f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res === 4f)
  }

  test("upsweep should correctly handle the chunk 1 until 4 of an array of 4 elements with threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, 1)
    assert(res === Node(Leaf(1,2,1f), Node(Leaf(2,3,4f), Leaf(3,4,3f))))
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList === List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight throws an IllegalArgumentException if output.length < input.length") {
    intercept[IllegalArgumentException] {
        parLineOfSight(Array[Float](0f, 1f, 8f, 9f), new Array[Float](1), 2)
      }
  }

  test("parLineOfSight throws an IllegalArgumentException if threshold <= 0") {
    intercept[IllegalArgumentException] {
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f), new Array[Float](6), 0)
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f), new Array[Float](6), -1)
    }
  }

  test("parLineOfSight should correctly handle an array of size 1") {
    val output = new Array[Float](1)
    parLineOfSight(Array[Float](0f), output, 1)
    assert(output.toList === List(0f))
  }

  test("parLineOfSight should correctly handle an array of size 2") {
    val output = new Array[Float](2)
    parLineOfSight(Array[Float](0f, 43f), output, 1)
    assert(output.toList === List(0f, 43f))
  }

  test("parLineOfSight should correctly handle an array of size 4 starting at 0") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    parLineOfSight(input, output, 1)
    assert(output.toList === List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight should correctly handle an array of size 4 starting at 2") {
    val input = Array[Float](2f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    parLineOfSight(input, output, 1)
    assert(output.toList === List(2f, 2f, 4f, 4f))
  }

}

