import scala.annotation.tailrec
import scala.io.Source

def day8(): Unit = {

  val input = Source.fromResource("day8.txt")

  val treeMap = input.getLines().toVector.foldLeft(Vector.empty[Vector[Int]]) {
    (agg, line) =>
      agg :+ line.split("").toVector.map(_.toInt)
  }

  val height = treeMap.length - 1
  val length = treeMap.head.length - 1

  val count = treeMap.zipWithIndex.foldLeft(0) { case (agg, (line, index)) =>
    line.zipWithIndex.foldLeft(agg) { case (agg, (_, lineIndex)) =>
      (index, lineIndex) match
        case (0, _) =>
          agg + 1
        case (_, 0) =>
          agg + 1
        case (`height`, _) =>
          agg + 1
        case (_, `length`) =>
          agg + 1
        case (_, _) =>
          if (isVisible(treeMap, index, lineIndex)) {
            agg + 1
          } else {
            agg
          }

    }
  }

  val scenicScores = treeMap.zipWithIndex.foldLeft(Vector.empty[Int]) {
    case (agg, (line, index)) =>
      agg ++ line.zipWithIndex.foldLeft(Vector.empty[Int]) {
        case (inner, (_, lineIndex)) =>
          (index, lineIndex) match
            case (0, _) =>
              inner :+ 0
            case (_, 0) =>
              inner :+ 0
            case (`height`, _) =>
              inner :+ 0
            case (_, `length`) =>
              inner :+ 0
            case (_, _) =>
              inner :+ scenicScore(treeMap, index, lineIndex)
      }
  }

  println(s"Day 8 part 1: $count")
  println(s"Day 8 part 2: ${scenicScores.max}")
}

def scenicScore(
    treeMap: Vector[Vector[Int]],
    index: Int,
    lineIndex: Int
): Int = {
  viewingDistance(
    0,
    treeMap,
    Position(index, lineIndex),
    Position(index, lineIndex - 1),
    Direction.Left
  ) * viewingDistance(
    0,
    treeMap,
    Position(index, lineIndex),
    Position(index, lineIndex + 1),
    Direction.Right
  ) * viewingDistance(
    0,
    treeMap,
    Position(index, lineIndex),
    Position(index + 1, lineIndex),
    Direction.Down
  ) * viewingDistance(
    0,
    treeMap,
    Position(index, lineIndex),
    Position(index - 1, lineIndex),
    Direction.Up
  )
}

@tailrec
def viewingDistance(
    score: Int,
    treeMap: Vector[Vector[Int]],
    position: Position,
    checkPosition: Position,
    direction: Direction
): Int = {

  val height = treeMap.length - 1
  val length = treeMap.head.length - 1

  val check = treeMap(position.index)(position.lineIndex) > treeMap(
    checkPosition.index
  )(
    checkPosition.lineIndex
  )

  val updatedScore = score + 1
  if (check) {
    direction match
      case Direction.Up =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (0, _) =>
            updatedScore
          case (_, _) =>
            viewingDistance(
              updatedScore,
              treeMap,
              position,
              checkPosition.copy(index = checkPosition.index - 1),
              direction
            )
      case Direction.Down =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (`height`, _) =>
            updatedScore
          case (_, _) =>
            viewingDistance(
              updatedScore,
              treeMap,
              position,
              checkPosition.copy(index = checkPosition.index + 1),
              direction
            )
      case Direction.Left =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (_, 0) =>
            updatedScore
          case (_, _) =>
            viewingDistance(
              updatedScore,
              treeMap,
              position,
              checkPosition.copy(lineIndex = checkPosition.lineIndex - 1),
              direction
            )
      case Direction.Right =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (_, `length`) =>
            updatedScore
          case (_, _) =>
            viewingDistance(
              updatedScore,
              treeMap,
              position,
              checkPosition.copy(lineIndex = checkPosition.lineIndex + 1),
              direction
            )
  } else {
    updatedScore
  }

}

@tailrec
def isVisibleImpl(
    treeMap: Vector[Vector[Int]],
    position: Position,
    checkPosition: Position,
    direction: Direction
): Boolean = {

  val height = treeMap.length - 1
  val length = treeMap.head.length - 1

  val check = treeMap(position.index)(position.lineIndex) > treeMap(
    checkPosition.index
  )(
    checkPosition.lineIndex
  )

  if (!check) {
    false
  } else {
    direction match
      case Direction.Up =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (0, _) =>
            check
          case (_, _) =>
            isVisibleImpl(
              treeMap,
              position,
              checkPosition.copy(index = checkPosition.index - 1),
              direction
            )
      case Direction.Down =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (`height`, _) =>
            check
          case (_, _) =>
            isVisibleImpl(
              treeMap,
              position,
              checkPosition.copy(index = checkPosition.index + 1),
              direction
            )

      case Direction.Left =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (_, 0) =>
            check
          case (_, _) =>
            isVisibleImpl(
              treeMap,
              position,
              checkPosition.copy(lineIndex = checkPosition.lineIndex - 1),
              direction
            )
      case Direction.Right =>
        (checkPosition.index, checkPosition.lineIndex) match
          case (_, `length`) =>
            check
          case (_, _) =>
            isVisibleImpl(
              treeMap,
              position,
              checkPosition.copy(lineIndex = checkPosition.lineIndex + 1),
              direction
            )
  }
}

def isVisible(
    treeMap: Vector[Vector[Int]],
    index: Int,
    lineIndex: Int
): Boolean = {
  isVisibleImpl(
    treeMap,
    Position(index, lineIndex),
    Position(index, lineIndex - 1),
    Direction.Left
  ) || isVisibleImpl(
    treeMap,
    Position(index, lineIndex),
    Position(index, lineIndex + 1),
    Direction.Right
  ) || isVisibleImpl(
    treeMap,
    Position(index, lineIndex),
    Position(index + 1, lineIndex),
    Direction.Down
  ) || isVisibleImpl(
    treeMap,
    Position(index, lineIndex),
    Position(index - 1, lineIndex),
    Direction.Up
  )

}

case class Position(index: Int, lineIndex: Int)

enum Direction:
  case Up
  case Down
  case Left
  case Right
