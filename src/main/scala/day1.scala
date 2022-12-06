def day1(): Unit = {
  val input = scala.io.Source.fromResource("day1.txt").getLines()
  val totals = input.foldLeft((LazyList.empty[Int], 0))((agg, line) => {
    line match
      case "" =>
        val first = agg._1 :+ agg._2
        (first, 0)
      case _ =>
        val second = agg._2 + line.toInt
        (agg._1, second)
  })

  val caloriesList = totals._1 :+ totals._2

  println(s"Day 1 part 1: ${caloriesList.max}")
  println(s"Day 1 part 2: ${caloriesList.sortWith(_ > _).take(3).sum}")
}
