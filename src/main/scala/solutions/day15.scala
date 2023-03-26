package solutions

import utils.IO._
import utils.Coord
import utils.Coord.given_Conversion_Long_Int

object day15 extends App {

  val ex  = readExample(15)
  println(ex)
  val xxx = readFile(15)
  println(xxx)

  val in                                   = xxx
  val sensors: List[(Coord, (Coord, Int))] = in.map {
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
      val sensor = Coord(sx.toInt, sy.toInt)
      val beacon = Coord(bx.toInt, by.toInt)
      (sensor, (beacon, sensor.manhattanDistance(beacon)))
  }
  val beaconCoords                         = sensors.map(_._2._1)

  val minX = sensors.map { case (s, (_, i)) => s.x - i }.min
  val maxX = sensors.map { case (s, (_, i)) => s.x + i }.max
  println(s"$minX => $maxX")

  val y = 2000000

  val vertical = Range.inclusive(minX, maxX).flatMap { x =>
    val c            = Coord(x, y)
    val canBeReached = sensors.exists { case (s, (_, i)) =>
      s.manhattanDistance(c) <= i
    }
    if (canBeReached) Some(c)
    else None
  }

  val withoutBeacons = vertical.filterNot(beaconCoords.contains)
  println(withoutBeacons.length)

  println("p1")

  println("p2")

  def walkDiagonal(start: Coord, distance: Int, xChange: Int, yChange: Int, max: Int = 4000000): Option[Coord] = {
    val oc: Option[Coord] = None
    val (_, found)        = Range.inclusive(1, distance).foldLeft((start, oc)) { case ((coord: Coord, found), i) =>
      if (found.isDefined) {
        (coord, found)
      } else if (coord.x < 0 || coord.y < 0 || coord.x > max || coord.y > max) {
        (coord.copy(coord.x + xChange, coord.y + yChange), found)
      } else {
        val isPickedUp = sensors.exists { case (s, (_, i)) =>
          s.manhattanDistance(coord) <= i
        }
        if (isPickedUp) (coord.copy(coord.x + xChange, coord.y + yChange), None)
        else (coord, Some(coord))
      }
    }
    found
  }

  def checkPerimeter(sensor: Coord, distance: Int): Option[Coord] = {
    lazy val top    = sensor.copy(y = sensor.y + (distance + 1))
    lazy val bottom = sensor.copy(y = sensor.y - (distance + 1))
    lazy val left   = sensor.copy(x = sensor.x - (distance + 1))
    lazy val right  = sensor.copy(x = sensor.x + (distance + 1))

    lazy val topRight    = walkDiagonal(top, distance + 1, 1, -1)
    lazy val rightBottom = walkDiagonal(right, distance + 1, -1, -1)
    lazy val bottomLeft  = walkDiagonal(bottom, distance + 1, -1, 1)
    lazy val leftTop     = walkDiagonal(left, distance + 1, 1, 1)

    lazy val perim = List(topRight, rightBottom, bottomLeft, leftTop)

    val found = perim.exists(_.isDefined)
    if (found) {
      Some(perim.flatten.head)
    } else None
  }

  val p2 = sensors.dropWhile { case (s, (_, i)) =>
    println(i)
    checkPerimeter(s, i).isEmpty
  }
    .take(1)
    .map { case (s, (_, i)) =>
      checkPerimeter(s, i)
    }
    .head
    .get

  val score = p2.x.toLong * 4000000 + p2.y
  println(score)

}
