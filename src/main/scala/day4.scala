import scala.io.Source

def day4(): Unit = {
  val input = Source.fromResource("day4.txt").getLines().toVector

  val (first, second) = input.foldLeft((0, 0)) {
    case ((firstTotal, secondTotal), line) =>
      val bounds =
        line.split(",").map { segment =>
          segment.split("-")
        }

      val firstRange = (bounds.head.head.toInt to bounds.head.last.toInt).toSet
      val secondRange = (bounds.last.head.toInt to bounds.last.last.toInt).toSet

      (
        (firstRange.subsetOf(secondRange) || secondRange.subsetOf(firstRange)),
        firstRange.intersect(secondRange).nonEmpty
      ) match
        case (true, true)   => (firstTotal + 1, secondTotal + 1)
        case (true, false)  => (firstTotal + 1, secondTotal)
        case (false, true)  => (firstTotal, secondTotal + 1)
        case (false, false) => (firstTotal, secondTotal)
  }

  println(s"Day 4 part 1: $first")
  println(s"Day 4 part 2: $second")
}
