package solutions

import utils.Coord
import utils.Coord.given_Conversion_Long_Int
import utils.Coord.given_Conversion_Int_Long
import utils.IO.*

object day9 extends App {

  val xxx = readFile(9)
//  val xxx = List("R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2")
//  val xxx = List("R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20")

  println(xxx)

  val directions: Map[Char, Coord] = Map(
    'R' -> Coord(1, 0),
    'L' -> Coord(-1, 0),
    'U' -> Coord(0, 1),
    'D' -> Coord(0, -1),
  )

  def chase(following: Coord, leading: Coord): Coord = {
    (following, leading) match {
      case (Coord(a, b), Coord(x, y)) if a < x && b + 1 < y => following + (1L, 1L)
      case (Coord(a, b), Coord(x, y)) if a + 1 < x && b < y => following + (1L, 1L)
      case (Coord(a, b), Coord(x, y)) if a > x && b - 1 > y => following + (-1L, -1L)
      case (Coord(a, b), Coord(x, y)) if a - 1 > x && b > y => following + (-1L, -1L)

      case (Coord(a, b), Coord(x, y)) if a < x && b - 1 > y => following + (1L, -1L)
      case (Coord(a, b), Coord(x, y)) if a + 1 < x && b > y => following + (1L, -1L)
      case (Coord(a, b), Coord(x, y)) if a > x && b + 1 < y => following + (-1L, 1L)
      case (Coord(a, b), Coord(x, y)) if a - 1 > x && b < y => following + (-1L, 1L)

      case (Coord(a, b), Coord(x, y)) if a == x && b + 1 < y => following + (0L, 1L)
      case (Coord(a, b), Coord(x, y)) if a == x && b - 1 > y => following + (0L, -1L)

      case (Coord(a, b), Coord(x, y)) if a + 1 < x && b == y => following + (1L, 0L)
      case (Coord(a, b), Coord(x, y)) if a - 1 > x && b == y => following + (-1L, 0L)

      case _ => following
    }

  }

  val p1 = Map(
    0 -> Coord(0, 0),
    1 -> Coord(0, 0),
  )

  val p2 = Map(
    0 -> Coord(0, 0),
    1 -> Coord(0, 0),
    2 -> Coord(0, 0),
    3 -> Coord(0, 0),
    4 -> Coord(0, 0),
    5 -> Coord(0, 0),
    6 -> Coord(0, 0),
    7 -> Coord(0, 0),
    8 -> Coord(0, 0),
    9 -> Coord(0, 0),
  )

  def run(knots: Map[Int, Coord]) = {

    val (visited, endKnots) = xxx.foldLeft((Set[Coord](), knots)) { case ((allH, currentKnots), instruction) =>
      val Array(dir, amount) = instruction.split(" ")
      List.fill(amount.toInt)(directions(dir.head)).foldLeft((allH, currentKnots)) { case ((allHH, kk), d) =>
        val newHead                   = kk(0) + d
        val headMoved                 = kk.updated(0, newHead)
        val allMoved: Map[Int, Coord] =
          Range.inclusive(0, kk.size - 2).toList.foldLeft(headMoved) { case (kkk, knot) =>
            val leading      = kkk(knot)
            val following    = kkk(knot + 1)
            val newFollowing = chase(following, leading)
            kkk.updated(knot + 1, newFollowing)
          }
        (allHH + allMoved(allMoved.size - 1), allMoved)
      }
    }
    (visited, endKnots)
  }

  private val p1ans: Int = run(p1)._1.size
  assert(p1ans == 6243)
  println(p1ans)

  private val p2ans: Int = run(p2)._1.size
  assert(p2ans == 2630)
  println(p2ans)

}
