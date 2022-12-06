def day2(): Unit = {
  val input = scala.io.Source.fromResource("day2.txt").getLines()
  val sum = input.to(LazyList).foldLeft((0, 0)) { (agg, line) =>
    val plays: Array[String] = line.split(" ")
    val other = parseToPlay(plays(0))
    val player = parseToPlay(plays(1))

    val score = parseToScore(plays(1))

    val scorePart1 = agg._1 + scoreRoundI(other, player)
    val scorePart2 = agg._2 + scoreRoundII(other, score)

    (scorePart1, scorePart2)
  }

  println(s"Day 2 part 1: ${sum._1}")
  println(s"Day 2 part 2: ${sum._2}")
}

def parseToPlay(value: String): Play = {
  value match
    case "A" | "X" => Play.Rock
    case "B" | "Y" => Play.Paper
    case "C" | "Z" => Play.Scissors
}

def parseToScore(value: String): Score = {
  value match
    case "X" => Score.Lose
    case "Y" => Score.Draw
    case "Z" => Score.Win
}

def scoreRoundII(other: Play, score: Score): Int = {
  (other, score) match {
    case (Play.Rock, Score.Win) =>
      Score.Win.value + Play.Paper.value
    case (Play.Rock, Score.Draw) =>
      Score.Draw.value + Play.Rock.value
    case (Play.Rock, Score.Lose) =>
      Score.Lose.value + Play.Scissors.value
    case (Play.Paper, Score.Win) =>
      Score.Win.value + Play.Scissors.value
    case (Play.Paper, Score.Draw) =>
      Score.Draw.value + Play.Paper.value
    case (Play.Paper, Score.Lose) =>
      Score.Lose.value + Play.Rock.value
    case (Play.Scissors, Score.Win) =>
      Score.Win.value + Play.Rock.value
    case (Play.Scissors, Score.Draw) =>
      Score.Draw.value + Play.Scissors.value
    case (Play.Scissors, Score.Lose) =>
      Score.Lose.value + Play.Paper.value
  }
}

def scoreRoundI(other: Play, player: Play): Int = {
  (other, player) match {
    case (Play.Rock, Play.Rock) =>
      Score.Draw.value + Play.Rock.value
    case (Play.Rock, Play.Paper) =>
      Score.Win.value + Play.Paper.value
    case (Play.Rock, Play.Scissors) =>
      Score.Lose.value + Play.Scissors.value
    case (Play.Paper, Play.Rock) =>
      Score.Lose.value + Play.Rock.value
    case (Play.Paper, Play.Paper) =>
      Score.Draw.value + Play.Paper.value
    case (Play.Paper, Play.Scissors) =>
      Score.Win.value + Play.Scissors.value
    case (Play.Scissors, Play.Rock) =>
      Score.Win.value + Play.Rock.value
    case (Play.Scissors, Play.Paper) =>
      Score.Lose.value + Play.Paper.value
    case (Play.Scissors, Play.Scissors) =>
      Score.Draw.value + Play.Scissors.value
  }
}

enum Play(val value: Int) {
  case Rock extends Play(1)
  case Paper extends Play(2)
  case Scissors extends Play(3)
}

enum Score(val value: Int) {
  case Win extends Score(6)
  case Lose extends Score(0)
  case Draw extends Score(3)
}
