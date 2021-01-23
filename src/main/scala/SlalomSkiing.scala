package  SlalomSkiing

//Task:  https://app.codility.com/programmers/lessons/90-tasks_from_indeed_prime_2015_challenge/slalom_skiing/
//Result:https://app.codility.com/demo/results/training84QGK3-FHU/

import scala.collection.Searching._
import scala.math.Ordering

object Solution {

  case class Candidate(tip: Int, len: Int )
  implicit def candidate1(tip:Int) = Candidate(tip, 1)

  val Inc:   Ordering[Candidate] = Ordering.by(_.tip)
  val Dec:   Ordering[Candidate] = Inc.reverse
  val ByLen: Ordering[Candidate] = Ordering.by(_.len)

  val empty:IndexedSeq[Candidate] = Vector()

  implicit class Candidates(self: IndexedSeq[Candidate]) {
    def len: Int = self.lastOption.fold(0)(_.len)

    def splitWhere(v: Candidate)(implicit ord: Ordering[Candidate]) = self.splitAt(self.search(v).insertionPoint)

    def dropTo(v: Candidate)(implicit ord: Ordering[Candidate]) = self.search(v) match {
      case Found(i) => self.drop(i+1)
      case InsertionPoint(i) => self.drop(i)
    }
  }

  case class LIS(candidates: IndexedSeq[Candidate], implicit val ordering: Ordering[Candidate]) {

    def + (nextEl: Int): LIS ={
      val (prefix,rest) = candidates.splitWhere(nextEl)
      val insert = Candidate(nextEl, prefix.len + 1)
      copy( (prefix :+ insert) ++ rest.dropTo(insert)(ByLen) )
    }

    def size: Int = candidates.len

    def ++ (incoming: LIS): LIS = {
      val insert = incoming.candidates.last
      val (prefix,rest) = candidates.splitWhere(insert)
      if (prefix.len >= insert.len) this
      else copy( (prefix :+ insert) ++ rest.dropTo(insert)(ByLen) )
    }
  }

  def solve(i: Int, a:Array[Int], straight:LIS = LIS(empty,Inc), turn1:LIS = LIS(empty,Dec), turn2:LIS = LIS(empty,Inc)): Int = {
    if(i >= a.length) turn2.size
    else {
      val s =  straight +  a(i)
      val t1 = if(s.size > straight.size) turn1 + a(i) ++ s  else turn1 + a(i)
      val t2 = if(t1.size > turn1.size)   turn2 + a(i) ++ t1 else turn2 + a(i)
      solve(i+1, a, s, t1, t2)
    }
  }

  def solution(a: Array[Int]): Int = {
      solve(0,a)
  }
}