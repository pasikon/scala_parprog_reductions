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
  ) withWarmer (new Warmer.Default)

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
    */
  def balance(chars: Array[Char]): Boolean = {

    var i = 0
    var res = 0
    var sub0Det = 0
    while (i < chars.length) {
      chars(i) match {
        case c: Char if c == '(' => res = res + 1
        case c: Char if c == ')' => res = res - 1
        case _ =>
      }
      i = i + 1
      if (res < 0) sub0Det = sub0Det - 1
    }
    res == 0 && !(sub0Det < 0)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, arg0: Int, arg1: Int): (Int, Int) = {

      var rP = 0
      var lP = 0
      var i = from
      while (i < until) {
        chars(i) match {
          case c: Char if c == '(' => lP = lP + 1
          case c: Char if c == ')' => if (lP > 0) lP = lP - 1 else rP = rP + 1
          case _ =>
        }
        i = i + 1
      }

      lP -> rP
    }


    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid: Int = from + (until - from) / 2
        val parallel1: ((Int, Int), (Int, Int)) = parallel(reduce(from, mid), reduce(mid, until))
        val tuple = math.max(0, parallel1._1._1 - parallel1._2._2 + parallel1._2._1) -> math.max(0, parallel1._1._2 + parallel1._2._2 - parallel1._1._1)
        tuple
      }

    }

    val reduce1 = reduce(0, chars.length)
    reduce1 == (0 -> 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
