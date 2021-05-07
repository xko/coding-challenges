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

  def ways(total: Int, k: Int): Int = firstPairs(total,k).size

  def firstPairs(total: Int, k: Int): List[(Int,Int)] = {
    if(total == 1) List((1,k))
    else firstPairs(total - 1, k) flatMap {
      case (a,b) if a+1 <= b => (1,a) :: (a+1,b) :: Nil
      case (a,_) => (1,a) :: Nil
    }
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
