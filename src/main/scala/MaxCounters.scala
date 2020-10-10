package MaxCounters
// Task:   https://app.codility.com/programmers/lessons/4-counting_elements/max_counters/
// Result: https://app.codility.com/demo/results/trainingNEBRJC-GYY/

case class Counter(N: Int, data: Map[Int, Int] = Map().withDefaultValue(0), curMax: Int = 0) {

  def +(el:Int):Counter  = el match {
    case el: Int if el <= N => Counter(N, data.updated(el, data(el) + 1), Math.max(data(el) + 1, curMax))
    case el: Int => Counter(N, Map().withDefaultValue(curMax), curMax)
  }

  def toArray: Array[Int] = Array.range(0,N).map(i => data(i+1))
}

object Solution {
  def solution(n: Int, a: Array[Int]): Array[Int] = a.foldLeft(Counter(n))( (c, el) => c + el ).toArray
}