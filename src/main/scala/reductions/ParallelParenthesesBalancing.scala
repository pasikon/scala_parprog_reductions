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
    def balAcc(chars: Array[Char], toBal: List[Char]): List[Char] = {
      chars.toList match {
        case Nil => toBal
        case he :: ta => he match {
          case c: Char if c == '(' => balAcc(ta.toArray, c :: toBal)
          case c: Char if c == ')' =>
            val toBalP = if (toBal.nonEmpty && toBal.head == '(') toBal.tail else c :: toBal
            balAcc(ta.toArray, toBalP)
          case _ => balAcc(ta.toArray, toBal)
        }
      }
    }

    if (chars.isEmpty) true else balAcc(chars, Nil).isEmpty
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {

      def balAcc(charsC: Array[Char], toBal: List[Char]): List[Char] = {
        charsC.toList match {
          case Nil => toBal
          case he :: ta => he match {
            case c: Char if c == '(' => balAcc(ta.toArray, c :: toBal)
            case c: Char if c == ')' =>
              val toBalP = if (toBal.nonEmpty && toBal.head == '(') toBal.tail else c :: toBal
              balAcc(ta.toArray, toBalP)
            case _ => balAcc(ta.toArray, toBal)
          }
        }
      }

      var lP = 0
      var rP = 0
      val unbalancedPars = balAcc(chars.slice(from, until), Nil)
      unbalancedPars.foldLeft()((a,b) => if (b == '(') lP = lP + 1 else rP = rP + 1)

      lP -> rP
    }


    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid: Int = from + (until - from) / 2
        val parallel1: ((Int, Int), (Int, Int)) = parallel(reduce(from, mid), reduce(mid, until))
        val tuple = (parallel1._1._1 - parallel1._2._2 + parallel1._2._1) -> (parallel1._1._2 + parallel1._2._2 - parallel1._1._1)
        tuple
      }

    }

    val reduce1 = reduce(0, chars.length)
    reduce1 == (0 -> 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
