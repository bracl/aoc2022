package utils

object IO {

  def readFile(day: Int, trim: Boolean = true): List[String] =
    readFile(s"/Users/bradleyking/dev/aoc2022/src/main/scala/input/$day.txt", trim)

  def readExample(day: Int, trim: Boolean = true): List[String] = readFile(
    s"/Users/bradleyking/dev/aoc2022/src/main/scala/input/$day.example.txt",
    trim,
  )

  def readFile(filename: String, trim: Boolean): List[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines          = (for (line <- bufferedSource.getLines()) yield if (trim) line.trim else line).toList
    bufferedSource.close
    lines
  }

  def readGrouped(day: Int): List[List[String]] = readGrouped(
    s"/Users/bradleyking/dev/aoc2022/src/main/scala/input/$day.txt",
  )

  def readGrouped(filename: String): List[List[String]] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines          = (for (line <- bufferedSource.getLines()) yield line.trim).toList
    val grouped        = lines
      .foldLeft((Vector[Vector[String]](), Vector[String]())) { case ((acc, currentGroup), next) =>
        if (next == "" || next == lines.last)
          val cg = next match {
            case "" => currentGroup
            case x  => currentGroup.appended(x)
          }
          (acc :+ cg, Vector())
        else (acc, currentGroup :+ next)
      }
      ._1
      .map(_.toList)
      .toList
    bufferedSource.close
    grouped
  }

  def readInts(day: Int): List[Int] = readInts(s"/Users/bradleyking/dev/aoc2022/src/main/scala/input/$day.txt")

  def readInts(filename: String): List[Int] = {
    val strings = readFile(filename, false)
    strings.map(s => s.toInt)
  }

  def updateMatrix[T](mat: List[List[T]], x: Int, y: Int, value: T): List[List[T]] =
    mat.updated(y, mat(y).updated(x, value))

  def printGrid[T](grid: List[List[T]], delim: String = ","): Unit =
    println(grid.map(v => v.mkString(delim)).mkString("\n") + "\n")

  def printGrid[T](grid: Vector[Vector[T]]): Unit =
    printGrid(grid.map(_.toList).toList)

  def updateMap[A, B](m: Map[A, B], key: A, value: B): Map[A, B] =
    m.removed(key).updated(key, value)

}
