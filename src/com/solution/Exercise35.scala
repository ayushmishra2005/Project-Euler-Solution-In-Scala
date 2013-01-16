package com.solution

import scala.collection.mutable.ListBuffer

/**
 * Solution of Project Euler Exercise 35
 * By Ayush Mishra
 */

/**
 * Problem :
 * The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 */

object Exercise35 {

  /*
   * Set Limit to One Million
   */
  val LIMIT = Math.pow(10, 6).toInt

  def getcirculerPrimes: Seq[Int] = {

    (for (i <- 2 to LIMIT if (isCirculerPrime(i))) yield i).filter(s => (s != 2 && s != 3 && s != 5 && s != 7))

  }

  def isPrime(num: Int): Boolean = {

    num == leastDivisor(num)
  }

  def leastDivisor(num: Int): Int = {

    findDivisor(num, 2)
  }

  def findDivisor(n: Int, divisior: Int): Int = {

    if (square(divisior) > n)
      n
    else if (divides(divisior, n))
      divisior
    else
      findDivisor(n, divisior + 1)
  }

  def square(n: Int): Int = {

    n * n
  }

  def divides(d: Int, n: Int): Boolean = {

    (n % d) == 0
  }

  def isCirculerPrime(num: Long) = {
    val str = num.toString()
    var cnt = 0
    for (i <- 0 until str.length) {
      if (isPrime((str.substring(i) + str.substring(0, i)).toInt)) cnt += 1
    }
    if (cnt == str.length()) true
    else false
  }

  def main(args: Array[String]) {
    println(getcirculerPrimes.size)

  }

}