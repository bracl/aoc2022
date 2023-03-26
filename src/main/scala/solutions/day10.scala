package solutions

import utils.IO._

object day10 extends App {

  val xxx               = readFile(10)
  println(xxx)
  val yyy: List[String] = xxx.flatMap {
    case s: String if s.contains("noop") => List(s)
    case s: String                       => List("noop", s)
  }
  println(yyy)

  val start = 1

  def process(i: String, x: Int): Int = {
    i match {
      case s: String if s.contains("noop") => x
      case s: String                       =>
        val amount: Int = s.split(" ").last.toInt
        x + amount

    }
  }

  val (_, mapp) = yyy.zipWithIndex.foldLeft((start, Map(1 -> 1))) { case ((x, m), (instruction, index)) =>
    val newX = process(instruction, x)
    (newX, m.updated(index + 1, newX))
  }

  println("p1")
  val indexes                    = List(20, 60, 100, 140, 180, 220)
  private val signals: List[Int] = indexes.map { case i: Int => mapp(i - 1) * i }
  println(signals)
  println(signals.sum)

  println("p2")
  val (_, _, listt) = yyy.zipWithIndex.foldLeft((start, Map(1 -> 1), List[String]())) {
    case ((x, m, l), (instruction, index)) =>
      val sprite = List(-1, 0, 1).map(x + _)
      val newX   = process(instruction, x)
      val out    = if (sprite.contains(index % 40)) l.appended("#") else l.appended(".")
      (newX, m.updated(index + 1, newX), out)
  }

  listt.grouped(40).foreach {
    println("\n")
    println(_)
  }

}
