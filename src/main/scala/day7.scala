import scala.collection.mutable
import scala.io.Source

type Path = List[String]
type Count = mutable.HashMap[Path, Int]

trait FileSystem

case class File(size: Int, name: String) extends FileSystem

case class Folder(
    files: mutable.HashMap[String, FileSystem],
    name: String,
    head: Path
) extends FileSystem

def day7(): Unit = {
  val (_, count) = interpreter()

  val totalSize = 70000000
  val requiredFreeSpace = 30000000
  val neededFreeSpace =
    requiredFreeSpace - (totalSize - count.toList.sortBy(_._2).reverse.head._2)

  println(s"Day 7 part 1: ${count.filter(_._2 <= 100000).values.sum}")
  println(s"Day 7 part 2: ${count.toList
    .sortBy(_._2)
    .reverse
    .filter(
      _._2 >= neededFreeSpace
    )
    .last
    ._2}")

}

def interpreter(): (FileSystem, Count) = {
  val input = Source.fromResource("day7.txt")
  val head = Folder(mutable.HashMap.empty, "/", List())
  val count = mutable.HashMap.empty[List[String], Int]

  input.getLines().toVector.tail.foldLeft((head, List("/"))) {
    case ((agg, path), line) =>
      val output = line.split(" ").toList

      output.head match {
        case "$" =>
          output.tail.head match
            case "cd" =>
              output.tail.last match
                case ".." =>
                  val updatedPath = path.dropRight(1)
                  (
                    updatedPath.foldLeft(head) { (agg, pos) =>
                      agg.files.getOrElse(pos, agg).asInstanceOf[Folder]
                    },
                    updatedPath
                  )
                case _ =>
                  (
                    agg.files
                      .getOrElse(output.tail.last, agg)
                      .asInstanceOf[Folder],
                    path :+ output.tail.last
                  )
            case "ls" => (agg, path)
        case "dir" =>
          agg.files.update(
            output.last,
            Folder(
              files = mutable.HashMap.empty,
              name = output.last,
              head = path
            )
          )

          (agg, path)
        case _ =>
          agg.files.update(
            output.last,
            File(
              name = output.last,
              size = output.head.toInt
            )
          )

          path.foldLeft(path) { (agg, _) =>
            val updateValue = count.getOrElseUpdate(agg, 0) + output.head.toInt
            count.update(agg, updateValue)
            agg.dropRight(1)
          }

          (agg, path)
      }
  }

  (head, count)
}
