package WaysToSum
// Task:   [see here](../resources/WaysToSum.html)
// Result: ...........

import java.io._
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.io._

object Result {
  val mod = 1000000007

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

    //allHeadPairs(total).size // slow, stays here for reference

    // If it starts with 1,1, we can only prepend another 1 - will not change the result
    // No need to look at such kind in future iterations, enough to count them
    @tailrec
    def dynamic(stable: Int, canIncManyWays:List[List[Int]], t: Int ):Int = {
      if(t==total) stable + canIncManyWays.size
      else {
        val considerNext = canIncManyWays.flatMap(waysToIncrement)
        dynamic( stable % mod + considerNext.count(_ == List(1, 1)),
                 considerNext.filterNot(_ == List(1,1)),
                 t+1
               )
      }
    }
    dynamic(0,List(List(1)),1)
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
