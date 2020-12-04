package day1
import scala.io.Source

object problem_1 {
  def main(args: Array[String]): Unit = {
    val input   = Source.fromResource("input.txt")
    val numbers = input.getLines().toList
    numbers.map(_.toInt).foldLeft(Set[Int]()) { case (dict: List[Int], num: Int) =>
      if (dict.contains(num)) {
        println(num, 2020 - num)
        println(num * (2020 - num))
        dict
      } else {
        dict ++ Set(2020 - num)
      }
    }
  }

}
