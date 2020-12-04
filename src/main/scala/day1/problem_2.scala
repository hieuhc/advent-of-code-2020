package day1
import scala.io.Source

object problem_2 {
  def main(args: Array[String]): Unit = {
    val input   = Source.fromResource("input.txt")
    val numbers = input.getLines().toList.map(_.toInt)
    def findTripleMul(lst: List[Int]): Int = {
      lst match {
        case _ :: Nil => 0
        case num :: tail =>
          val tailRes = findTripleMul(tail)
          if (tailRes != 0)
            tailRes
          else {
            val trialNum = sumOfTwoExist(tail, 2020 - num)
            if (trialNum != 0) {
              println(trialNum, num, 2020 - trialNum - num)
            }
            trialNum * num * (2020 - trialNum - num)
          }
      }
    }
    println(findTripleMul(numbers))
  }

  def sumOfTwoExist(lst: List[Int], sum: Int): Int = {
    lst.foldLeft((List[Int](), 0)) { case ((neededLst, numFound), num) =>
      if (numFound != 0) {
        (neededLst, numFound)
      } else if (neededLst.contains(num)) {
        (neededLst, num)
      } else {
        (neededLst ++ List(sum - num), numFound)
      }
    }._2
  }

}
