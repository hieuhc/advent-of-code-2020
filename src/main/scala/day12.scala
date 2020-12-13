import scala.io.Source
import scala.math._
import cats.implicits._

object day12 {
  def main(args: Array[String]): Unit = {
    val input: List[(Char, Int)] =
      Source.fromResource("input_day12.txt").getLines().toList.map(str => (str.head, str.tail.toInt))
    def turn(order: String, init: Char, degree: Int): Char = order((order.indexOf(init) + (degree / 90)) % 4)
    val sol1 = input.foldLeft(('E', Map('E' -> 0, 'N' -> 0, 'W' -> 0, 'S' -> 0))) {
      case ((crrDir: Char, crrPos: Map[Char, Int]), (action: Char, offset: Int)) =>
        action match {
          case 'F' => (crrDir, crrPos.combine(Map(crrDir -> offset)))
          case 'L' => (turn("ENWS", crrDir, offset), crrPos)
          case 'R' => (turn("ESWN", crrDir, offset), crrPos)
          case _   => (crrDir, crrPos.combine(Map(action -> offset)))
        }
    }._2
    println(abs(sol1('E') - sol1('W')) + abs(sol1('N') - sol1('S')))

    def turnWaypoint(order: String, init: Map[Char, Int], degree: Int): Map[Char, Int] = {
      "NSEW".map { ch: Char => (turn(order, ch, degree), init(ch)) }.toMap
    }
    val sol2 =
      input.foldLeft((Map('E' -> 10, 'N' -> 1, 'S' -> 0, 'W' -> 0)), Map('E' -> 0, 'N' -> 0, 'S' -> 0, 'W' -> 0)) {
        case ((wayPoint: Map[Char, Int], crrPos: Map[Char, Int]), (action: Char, offset: Int)) =>
          action match {
            case 'F' => (wayPoint, crrPos.map { case (ch, value) => (ch, value + offset * wayPoint(ch)) })
            case 'L' => (turnWaypoint("ENWS", wayPoint, offset), crrPos)
            case 'R' => (turnWaypoint("ESWN", wayPoint, offset), crrPos)
            case _   => (wayPoint.combine(Map(action -> offset)), crrPos)
          }
      }._2
    println(abs(sol2('E') - sol2('W')) + abs(sol2('N') - sol2('S')))
  }
}
