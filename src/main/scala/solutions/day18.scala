package solutions

import utils.IO.*

import scala.annotation.tailrec

object day18 extends App {

  val ex  = readExample(18)
  println(ex)
  val xxx = readFile(18)
  println(xxx)

  val directions: List[Coord] = List(
    Coord(1, 0, 0),
    Coord(-1, 0, 0),
    Coord(0, 1, 0),
    Coord(0, -1, 0),
    Coord(0, 0, 1),
    Coord(0, 0, -1),
  )

  case class Coord(x: Int, y: Int, z: Int) {

    def add(c: Coord): Coord = Coord(x + c.x, y + c.y, z + c.z)

    def surrounding(): List[Coord] = directions.map(d => this.add(d))

    def surroundingAir(s: Set[Coord]): List[Coord] = surrounding().filter(c => !s.contains(c))

    def touchingLava(s: Set[Coord]): Int = this.surrounding().count(s.contains)
    def exposedSides(s: Set[Coord]): Int = 6 - this.touchingLava(s)

    def buildPocket(s: Set[Coord], x: Int = 20): Set[Coord] = {
      val start = Set(this)

      val pp = Range.inclusive(1, x).foldLeft(start) { case (air, _) =>
        val potential = air ++ air.flatMap(_.surroundingAir(s))
        if (
          potential.nonEmpty && (
            potential.maxBy(_.x).x > maxX ||
              potential.minBy(_.x).x < minX ||
              potential.maxBy(_.y).y > maxY ||
              potential.maxBy(_.y).y < minY ||
              potential.maxBy(_.z).z > maxZ ||
              potential.maxBy(_.z).z < minZ
          )
        ) Set()
        else potential
      }

      if (pp.isEmpty) Set()
      else pp
    }

    def internalPocketByWalking(s: Set[Coord]): Set[Coord] = if (s.contains(this)) Set() else this.buildPocket(s)
  }

  val in = xxx

  val coords = in.foldLeft(Set[Coord]()) { case (coords, c) =>
    c match {
      case s"$x,$y,$z" => coords + Coord(x.toInt, y.toInt, z.toInt)
    }
  }
  println(coords)
  val maxX   = coords.maxBy(_.x).x
  val minX   = coords.minBy(_.x).x
  val maxY   = coords.maxBy(_.y).y
  val minY   = coords.minBy(_.y).y
  val maxZ   = coords.maxBy(_.z).z
  val minZ   = coords.minBy(_.z).z

  println("p1")
  println(coords.toList.map(c => c.exposedSides(coords)).sum)

  println("p2")
  val (sides, _, _) = coords.toList.foldLeft((0, Set[Coord](), 1)) { case ((count, pockets, i), coord) =>
    println(s"${i - 1}, $count")

    val air            = coord.surroundingAir(coords).toSet
    val updatedPockets = pockets ++ air.diff(pockets).flatMap(a => a.internalPocketByWalking(coords))
    val exposedSides   = air.diff(updatedPockets)

    (count + exposedSides.size, updatedPockets, i + 1)
  }
  println(sides)

}
