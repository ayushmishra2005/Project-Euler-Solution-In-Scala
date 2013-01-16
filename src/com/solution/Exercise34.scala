package com.solution

/**
 * Solution of Project Euler Exercise 34
 * By Ayush Mishra
 */

/**
 * 	145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of their digits.
 *
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 */

object Exercise34 {

  val LIMIT = Math.pow(10, 4).toInt

  def fact(n: Int): Int = if (n == 1 || n == 0) 1 else n * fact(n - 1)

  def getSum = {
    var sum = 0
    getFactorialOfDigits.map(n => sum += n)
    sum
  }

  def getFactorialOfDigits: Seq[Int] = {
    (for (i <- 2 until LIMIT if (i == getCirculerFact(i))) yield i)
  }

  /*
   * Get Factorial of all Digits of a number
   */
  def getCirculerFact(num: Int) = {
    val str = num.toString()
    var numSum = 0
    str.map { num => numSum += fact(num.toString().toInt) }
    numSum
  }

  def main(args: Array[String]) {
    println(getSum)
  }
}