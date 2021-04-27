package SockMerchant
// Task:   https://www.hackerrank.com/challenges/sock-merchant/
// Result: https://www.hackerrank.com/challenges/sock-merchant/submissions/code/210773550

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

object Result {

  /*
   * Complete the 'sockMerchant' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER n
   *  2. INTEGER_ARRAY ar
   */

  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    ar.groupBy(identity).map(_._2.length / 2).sum
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = StdIn.readLine.trim.toInt

    val ar = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)

    val result = Result.sockMerchant(n, ar)

    printWriter.println(result)

    printWriter.close()
  }
}
