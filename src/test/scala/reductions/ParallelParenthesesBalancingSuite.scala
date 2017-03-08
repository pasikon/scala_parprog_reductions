package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("balance PAR should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 10) == expected,
        s"balance($input) should be $expected")

    check("(())", true)
    check("(())ijoijoijoji(((())))", true)
    check("(())ijoijoijoji(((())))", true)
    check("(())ijoijoijoji(((()))()", false)
    check("()))))))))))))(((((((((((()))))))))))((((((((((()))))))))(((((((((((())))))))))((((" +
      "(((((())))))))))(((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))", false)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("pb 2 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check("(())", true)
  }


}