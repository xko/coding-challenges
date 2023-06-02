package WaysToSum
// Task:   [see here](../resources/WaysToSum.html)
// Result: ...........

import java.io._
import scala.collection.immutable._
import scala.io._

object Result {

  /*
   * Complete the 'ways' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER total
   *  2. INTEGER k
   */

  def ways(total: Int, k: Int): Int = {

    // There are 2 ways:
    // - prepend 1 - always possible
    // - increment one element -  only the head and only if next one is greater
    //   (keeps it sorted to avoid counting permutations)
    // Means we only ever need to see up to 2 head elements
    def waysToIncrement(headPair: List[Int]): List[List[Int]] = headPair match {
      case a :: b :: Nil if a < b => List(List(1, a), List(a + 1, b))
      case a :: Nil      if a < k => List(List(1, a), List(a + 1))
      case a :: _                 => List(List(1, a))
    }
    def allHeadPairs(total: Int): List[List[Int]] = {
      if(total==1) List(List(1))
      else allHeadPairs(total-1).flatMap(waysToIncrement)
    }
    allHeadPairs(total).size
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val total = StdIn.readLine.trim.toInt

    val k = StdIn.readLine.trim.toInt

    val result = Result.ways(total, k)

    printWriter.println(result)

    printWriter.close()
  }
}
