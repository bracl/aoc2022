package solutions

import utils.IO._

object day2 extends App {

  val xxx = readFile(2)

  def play(p1: String, p2: String): Int = {
    (p1, p2) match {
      case ("A", "X") => 1 + 3
      case ("A", "Y") => 2 + 6
      case ("A", "Z") => 3 + 0
      case ("B", "X") => 1 + 0
      case ("B", "Y") => 2 + 3
      case ("B", "Z") => 3 + 6
      case ("C", "X") => 1 + 6
      case ("C", "Y") => 2 + 0
      case ("C", "Z") => 3 + 3
    }
  }

  def playTwo(p1: String, outcome: String): Int = {
    (outcome, p1) match {
      case ("X", "A") => play(p1, "Z")
      case ("Y", "A") => play(p1, "X")
      case ("Z", "A") => play(p1, "Y")
      case ("X", "B") => play(p1, "X")
      case ("Y", "B") => play(p1, "Y")
      case ("Z", "B") => play(p1, "Z")
      case ("X", "C") => play(p1, "Y")
      case ("Y", "C") => play(p1, "Z")
      case ("Z", "C") => play(p1, "X")
    }
  }

  val (p1, p2) = xxx.foldLeft((0, 0)) { case ((p1, p2), game) =>
    val (opp: String, mine: String) = game.split(" ").toList match { case a :: b :: Nil => (a, b) }
    (p1 + play(opp, mine), p2 + playTwo(opp, mine))
  }

  println(p1)
  println(p2)

}
