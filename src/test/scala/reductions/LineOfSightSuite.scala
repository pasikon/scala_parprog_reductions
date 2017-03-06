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
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight 2 should correctly handle an array of size 4") {
    val output = new Array[Float](5)
    lineOfSight(Array[Float](0f, 7f, 8f, 33f, 48f), output)
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    val input = Array[Float](0f, 1f, 8f, 9f)
    val tree = upsweep(input, 0, input.length, 4)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle a 5 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    val input = Array[Float](0f, 1f, 8f, 9f, 7f)
    val tree = upsweep(input, 0, input.length, 5)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 1f, 4f, 4f, 4f))
  }

  test("downsweep 2 should correctly handle a 5 element thr 5 array when the starting angle is zero") {
    val output = new Array[Float](5)
    val input = Array[Float](0f, 7f, 8f, 33f, 48f)
    val tree = upsweep(input, 0, input.length, 5)
    downsweep(input, output, 0f, tree)
    println(output.toList)
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
  }

  test("downsweep 3 should correctly handle a 5 element thr 2 array when the starting angle is zero") {
    val output = new Array[Float](5)
    val input = Array[Float](0f, 7f, 8f, 33f, 48f)
    val tree = upsweep(input, 0, input.length, 2)
    downsweep(input, output, 0f, tree)
    println(output.toList)
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
  }


}

