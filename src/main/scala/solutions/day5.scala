package solutions

import utils.IO._

object day5 extends App {

  val xxx          = readFile(5).takeWhile(s => !s.contains("move"))
  val instructions = readFile(5).dropWhile(s => !s.contains("move"))
  println(xxx)
  println(instructions)

  val crates: Map[Int, Vector[Char]] = Map(
    1 -> Vector('C', 'S', 'G', 'B').reverse,
    2 -> Vector('G', 'V', 'N', 'J', 'H', 'W', 'M', 'T').reverse,
    3 -> Vector('S', 'Q', 'M').reverse,
    4 -> Vector('M', 'N', 'W', 'T', 'L', 'S', 'B').reverse,
    5 -> Vector('P', 'W', 'G', 'V', 'T', 'F', 'Z', 'J').reverse,
    6 -> Vector('S', 'H', 'Q', 'G', 'B', 'T', 'C').reverse,
    7 -> Vector('W', 'B', 'P', 'J', 'T').reverse,
    8 -> Vector('M', 'Q', 'T', 'F', 'Z', 'C', 'D', 'G').reverse,
    9 -> Vector('F', 'P', 'B', 'H', 'S', 'N').reverse,
  )

  val moved = instructions.foldLeft((crates, crates)) { case ((current, current9001), instruction) =>
    val (howMany, source, target) = instruction match {
      case s"move $x from $y to $z" => (x.toInt, y.toInt, z.toInt)
    }
    val toMove                    = current(source).takeRight(howMany).reverse
    val toMove9001                = current9001(source).takeRight(howMany)
    val newStack                  = current(target) ++ toMove
    val newStack9001              = current9001(target) ++ toMove9001
    val oldStack                  = current(source).dropRight(howMany)
    val oldStack9001              = current9001(source).dropRight(howMany)
    (
      current.updated(target, newStack).updated(source, oldStack),
      current9001.updated(target, newStack9001).updated(source, oldStack9001),
    )
  }

  println("p1")
  println(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9).map(moved._1(_).last).mkString(""))

  println("p2")
  println(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9).map(moved._2(_).last).mkString(""))

}
