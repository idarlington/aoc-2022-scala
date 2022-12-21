import scala.collection.mutable.ListBuffer
import scala.io.Source

def day6(): Unit = {
  val input = Source.fromResource("day6.txt").mkString.toCharArray.toVector

  println(s"Day 6 part 1: ${findMarker(4, input)}")
  println(s"Day 6 part 2: ${findMarker(14, input)}")
}

def findMarker(size: Int, input: Vector[Char]): Int = {
  var list = ListBuffer.empty[(Char, Int)]

  input.zipWithIndex
    .takeWhile { case (char, index) =>
      list += ((char, index + 1))

      if (index >= (size - 1)) {
        if (list.map(_._1).toSet.size == size) {
          false
        } else {
          list = list.tail
          true
        }
      } else {
        true
      }
    }

  list.last._2
}
