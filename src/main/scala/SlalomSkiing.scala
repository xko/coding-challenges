package  SlalomSkiing

//Task:  https://app.codility.com/programmers/lessons/90-tasks_from_indeed_prime_2015_challenge/slalom_skiing/
//Result:https://app.codility.com/demo/results/trainingDNG57N-69J/

import scala.collection.SortedSet
import scala.language.implicitConversions


object Solution {

  case class DLong(v:Long, d: Int=0) extends Ordered[DLong] {

    override def compare(that: DLong): Int = if(v==that.v) d.compare(that.d) else v.compare(that.v)

    def nudge(dd: Int) : DLong = copy(v,d+dd)
  }

  val Inc:SortedSet[DLong] = SortedSet.empty
  val Dec:SortedSet[DLong] = SortedSet.empty(Ordering[DLong].reverse)

  implicit def llong(v: Long) = DLong(v)

  case class LIS(tails: SortedSet[DLong] ) {
    val decrement:Int = tails.ordering.min(1,-1).v.toInt

    def + (nextEl: Long): LIS = copy(tails.until(nextEl) + nextEl ++ tails.from(nextEl).drop(1))
    def size: Int = tails.size

    def fill(to:DLong, fromSize: Int, toSize: Int): Seq[DLong] =  1.until(toSize - fromSize).map(_ * decrement).map(to.nudge)

    def ++ (incoming: LIS): LIS = {
      val insert = incoming.tails.last
      val prefix = tails.until(insert)
      val filler = fill(insert, prefix.size, incoming.size)
      val rest = tails.drop(prefix.size+filler.size+1)
      copy(prefix ++ filler + insert ++ rest)
    }

  }


  def solve(i: Int, N:Int, a: Int=>Long, straight:LIS = LIS(Inc), turn1:LIS = LIS(Dec), turn2:LIS = LIS(Inc)): Int = {
    if(i >= N) turn2.size
    else {
      val s =  straight +  a(i)
      val t1 = turn1    +  a(i) ++ s
      val t2 = turn2    +  a(i) ++ t1
      solve(i+1, N, a, s, t1, t2)
    }
  }

  def solution(a: Array[Int]): Int = {
      solve(0,a.length,i=>a(i))
  }
}