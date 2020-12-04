import scala.io.Source

object day2 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input_day2.txt").getLines().toList
    val parseLst = lines.map(str => {
      val (policy, password) = str.split(": ") match {
        case Array(po, pa) => (po, pa)
      }
      val (range, char) = policy.split(" ") match {
        case Array(ra, cha) => (ra, cha)
      }
      val (min, max) = range.split("-") match {
        case Array(mi, ma) => (mi, ma)
      }
      (min.toInt, max.toInt, char.head, password)
    })
    println(parseLst.count(tup => isValidPassPart1(tup._1, tup._2, tup._3, tup._4)))
    println(parseLst.count(tup => {
      val str      = tup._4
      val charPos1 = str.charAt(tup._1 - 1)
      val charPos2 = str.charAt(tup._2 - 1)
      (charPos1 == tup._3 && charPos2 != tup._3) || (charPos1 != tup._3 && charPos2 == tup._3)
    }))
    def isValidPassPart1(min: Int, max: Int, char: Char, password: String): Boolean = {
      val countChar = password.foldLeft(0) { case (count, crrChar) =>
        if (crrChar == char) {
          count + 1
        } else {
          count
        }
      }
      countChar >= min && countChar <= max
    }
  }
}
