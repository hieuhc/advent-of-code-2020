import scala.io.Source

object day7 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input_day7.txt").getLines().toList
    def extractGraph(mapFunc: (String, Set[(Int, String)]) => Map[String, Set[(Int, String)]])
        : Map[String, Set[(Int, String)]] =
      input.foldLeft(Map[String, Set[(Int, String)]]()) { case (crrGraph, line) =>
        val (bagContainer, bagContainedList) = line match {
          case s"${container} bags contain ${containedListStr}." =>
            (
              container,
              containedListStr.split(", ").collect {
                case s"${num} ${contained} bag${_}" if contained != "other" => (num.toInt, contained)
              })
        }
        val lineMap = mapFunc(bagContainer, bagContainedList.toSet)
        crrGraph ++ lineMap.map { case (k, v) => k -> (v ++ crrGraph.getOrElse(k, Set[(Int, String)]())) }
      }
    val containerGraph: Map[String, Set[(Int, String)]] = extractGraph((a, b) => Map(a -> b))
    println(containerGraph)
    val cachedContainer = scala.collection.mutable.Map[String, Int]()
    def countBag(bag: String): Int = {
      if (cachedContainer.contains(bag)) cachedContainer(bag)
      else {
        val countChild =
          containerGraph.getOrElse(bag, Set[(Int, String)]()).foldLeft(0) { case (crrCount, (childNum, childBag)) =>
            crrCount + childNum * countBag(childBag)
          }
        val res = countChild + 1
        cachedContainer.addOne((bag, res))
        res
      }
    }
    println(countBag("shiny gold") - 1)

    val containedGraph       = extractGraph((a, b) => b.map(tup => (tup._2, Set((0, a)))).toMap)
    var visited: Set[String] = Set()
    var count                = 0
    def countContainer(bag: String): Unit = {
      if (!visited.contains(bag)) {
        visited = visited ++ Set(bag)
        count += 1
        for (child <- containedGraph.getOrElse(bag, Set())) {
          countContainer(child._2)
        }
      }
    }
    countContainer("shiny gold")
    println(count - 1)
  }

}
