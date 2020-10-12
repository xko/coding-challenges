package  SlalomSkiing

import scala.collection.SortedSet


object Solution {

  case class LIS(tails: SortedSet[Int] = SortedSet.empty) {
    def + (nextEl: Int): LIS = copy(tails.until(nextEl) + nextEl ++ tails.from(nextEl).drop(1))
    def size: Int = tails.size
  }

  def solve(i: Int, N:Int, a: Int=>Int, turnsToGo: Int, lis: LIS = LIS()): Int = {
    if(i >= N) lis.size
    else {
      val straight = solve(i+1,N,a,turnsToGo,lis+a(i))
      if (turnsToGo == 0) straight else {
        val turned = solve(i + 1, N, j => 2 * a(i) - a(j), turnsToGo - 1, lis + a(i))
        Math.max(straight, turned)
      }
    }
  }

  def solution(a: Array[Int]): Int = {
    solve(0,a.length,a,2)
  }
}