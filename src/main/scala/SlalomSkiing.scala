package  SlalomSkiing

//Task:  https://app.codility.com/programmers/lessons/90-tasks_from_indeed_prime_2015_challenge/slalom_skiing/
//Result:https://app.codility.com/demo/results/trainingBS5KPC-2KM/

import scala.collection.SortedSet


object Solution {

  case class LIS(tails: SortedSet[Long] = SortedSet.empty) {
    def + (nextEl: Long): LIS = copy(tails.until(nextEl) + nextEl ++ tails.from(nextEl).drop(1))
    def size: Int = tails.size
  }

  def solve(i: Int, N:Int, a: Int=>Long, turnsToGo: Int, lis: LIS = LIS()): Int = {
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
      solve(0,a.length,i=>a(i) ,2)
  }
}