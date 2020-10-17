package  SlalomSkiing

//Task:  https://app.codility.com/programmers/lessons/90-tasks_from_indeed_prime_2015_challenge/slalom_skiing/
//Result:https://app.codility.com/demo/results/trainingDNG57N-69J/

import scala.collection.Searching._
import scala.math.Ordering

object Solution {

  case class DLong(v:Long, d: Int=0) extends Ordered[DLong] {
    override def compare(that: DLong): Int = if(v==that.v) d.compare(that.d) else v.compare(that.v)
    def nudge(dd: Int) : DLong = copy(v,d+dd)
  }

  val minus0 = DLong(0,-1)
  val plus0 = DLong(0,1)


  val Inc = Ordering[DLong]
  val Dec = Ordering[DLong].reverse
  val empty:IndexedSeq[DLong] = Nil.toIndexedSeq

  case class LIS(tails: IndexedSeq[DLong], implicit val ordering: Ordering[DLong]) {
    val decrement:Int = ordering.min(minus0,plus0).d

    def + (nextEl: Long): LIS ={
      val ip = tails.search(DLong(nextEl)).insertionPoint
      copy((tails.slice(0, ip) :+ DLong(nextEl)) ++ tails.drop(ip+1))
    }
    def size: Int = tails.size

    def fill(to:DLong, fromSize: Int, toSize: Int): Seq[DLong] =  1.until(toSize - fromSize).map(_ * decrement).map(to.nudge)

    def ++ (incoming: LIS): LIS = {
      val insert = incoming.tails.last
      val ip = tails.search(insert).insertionPoint
      val prefix = tails.slice(0, ip)
      val filler = fill(insert, prefix.size, incoming.size)
      val rest = tails.drop(prefix.size+filler.size+1)
      copy((prefix ++ filler :+ insert) ++ rest)
    }

  }

  def solve(i: Int, N:Int, a: Int=>Long, straight:LIS = LIS(empty,Inc), turn1:LIS = LIS(empty,Dec), turn2:LIS = LIS(empty,Inc)): Int = {
    if(i >= N) turn2.size
    else {
      val s =  straight +  a(i)
      val t1 = if(s.size > straight.size) turn1 ++ s  else turn1 + a(i)
      val t2 = if(t1.size > turn1.size)   turn2 ++ t1 else turn2 + a(i)
      solve(i+1, N, a, s, t1, t2)
    }
  }

  def solution(a: Array[Int]): Int = {
      solve(0,a.length,i=>a(i))
  }
}