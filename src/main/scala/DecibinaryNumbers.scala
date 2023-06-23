package DecibinaryNumbers
// Task:   https://www.hackerrank.com/challenges/decibinary-numbers/
// Result: https://www.hackerrank.com/challenges/decibinary-numbers/submissions/code/332919163

import java.io._
import scala.annotation.tailrec
import scala.io._
import scala.math.min

object Result {
    type DB = List[Int] // digits, head is least significant

    def fromLong(digits: Long):DB = digits.toString.reverse.map(_.toString.toInt).toList

    def toLong(db: DB): Long = db.reverse.mkString.toLong

    def increment(db: DB): List[DB] = db match {
        case Nil => Nil
        case 9 :: _ => Nil
        case 1 :: Nil => (2 :: Nil) :: (0 :: 1 :: Nil) :: Nil
        case 1 :: etc => (2 :: etc) :: increment(etc).map(0 :: _)
        case d :: etc => (d + 1 :: etc) :: Nil
    }


    def dbinsPerValue(v: Long): Long =
        if (v < 2) 1
        else (0L to min(v / 2, 4)).map(d => dbinsPerValue(v / 2 - d)).sum


    def decibinaryNumbers(x: Long): Long = {
        @tailrec
        def solve(totalBeforeLast: Long, last:List[DB]):DB = {
            if(totalBeforeLast + last.length >= x) last((x-totalBeforeLast).toInt-1)
            else solve(totalBeforeLast+last.length, last.flatMap(increment))
        }
        if(x==1) 0 else toLong(solve(1, List(List(1))))
    }

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

