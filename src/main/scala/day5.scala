import scala.io.Source

def day5(): Unit = {
  val input = Source.fromResource("day5.txt").mkString.split("\n\n")

  val stack = input(0)
  val instructionsString = input(1)

  val mapping = stack
    .split("\r?\n")
    .foldLeft(
      (Vector.empty[Vector[String]])
    ) { case (mapping, line) =>
      val splits = line
        .split("")
        .grouped(4)
        .toVector
        .map { sector =>
          sector.filter(_.matches("[A-Z]+")).toVector
        }

      splits.zipWithIndex.foldLeft(mapping) { case (agg, (split, index)) =>
        if (agg.unapply(index).nonEmpty) {
          agg.updated(
            index,
            agg(index) ++ split
          )
        } else {
          agg :+ split
        }
      }
    }

  val instructions = instructionsString.split("\r?\n").map { line =>
    val lineInstructions =
      line.split(" ").filter(_.matches("[1-9].*")).toVector
    Instructions(
      lineInstructions(0).toInt,
      lineInstructions(1).toInt - 1,
      lineInstructions(2).toInt - 1
    )
  }

  val (first, second) = instructions.foldLeft((mapping, mapping)) {
    case ((first, second), instruction) =>
      (
        first
          .updated(
            instruction.next,
            first(instruction.current)
              .take(instruction.amount)
              .reverse ++ first(
              instruction.next
            )
          )
          .updated(
            instruction.current,
            first(instruction.current).drop(instruction.amount)
          ),
        second
          .updated(
            instruction.next,
            second(instruction.current)
              .take(instruction.amount)
              ++ second(
                instruction.next
              )
          )
          .updated(
            instruction.current,
            second(instruction.current).drop(instruction.amount)
          )
      )
  }

  println(s"Day 5 part 1: ${first.map(_.headOption.getOrElse(" ")).mkString}")
  println(s"Day 5 part 2: ${second.map(_.headOption.getOrElse(" ")).mkString}")
}

case class Instructions(amount: Int, current: Int, next: Int)
