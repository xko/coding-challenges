package DecibinaryNumbers

import java.io._
import scala.io._
import scala.math.min

object Result {
  type Digits = List[Int]

  def dbinsPerValue(v: Long):Long =
    if (v < 2) 1
    else (0L to min(v/2,4)).map(d => dbinsPerValue(v/2 - d)).sum


  def decode(db:Digits):Long = db match {
    case Nil => 0
    case d::rest => d + 2*decode(rest)
  }


  def decibinaryNumbers(x: Long): Long = ???

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val q = StdIn.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val x = StdIn.readLine.trim.toLong

      val result = Result.decibinaryNumbers(x)

      printWriter.println(result)
    }

    printWriter.close()
  }
}

