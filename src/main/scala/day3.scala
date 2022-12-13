import scala.io.Source

val priority: Map[Char, Int] =
  (('a' to 'z').zip(1 to 26) ++ ('A' to 'Z').zip(27 to 52)).toMap

def day3(): Unit = {
  val input = Source.fromResource("day3.txt")
  val lines: Vector[String] = input.getLines().to(Vector)

  val total = lines.foldLeft(0) { (agg, line) =>
    val (first, second) = line.splitAt(line.length / 2)
    val intersect = first.toSet.intersect(second.toSet)

    agg + sum(intersect)
  }

  val groupTotal = lines.grouped(3).toVector.foldLeft(0) { (agg, group) =>
    val intersect =
      group(0).toSet.intersect(group(1).toSet).intersect(group(2).toSet)

    agg + sum(intersect)
  }

  println(s"Day 3 part 1: $total")
  println(s"Day 3 part 2: $groupTotal")
}

def sum(intersect: Set[Char]): Int = {
  intersect.foldLeft(0) { (sum, char) =>
    sum + priority.getOrElse(char, 0)
  }
}
