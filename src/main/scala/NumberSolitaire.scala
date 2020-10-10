package NumberSolitaire
// Task:   https://app.codility.com/programmers/lessons/17-dynamic_programming/number_solitaire/
// Result: https://app.codility.com/demo/results/trainingGE27KH-YHZ/

object Solution {

  case class Game(a: Array[Int], till: Int, before: Array[Int] = Array()) {
    def next(): Game = Game(a, till + 1, (before :+ this.answer).takeRight(6))

    val answer: Int = if(before.isEmpty) a(till) else before.map(_ + a(till)).max
  }


  def solution(a: Array[Int]): Int = {
    val res = (1 until a.length).foldLeft ( Game(a, 0) )((game, _) => game.next() )
    res.answer
  }
}