package solutions

import utils.IO.*

import scala.annotation.tailrec

object day14 extends App {

  val xxx = readFile(14)
  println(xxx.head)

  case class Coord(x: Int, y: Int) {
    def above: Coord     = this.copy(x, y - 1)
    def downLeft: Coord  = this.copy(x - 1, y + 1)
    def downRight: Coord = this.copy(x + 1, y + 1)
  }

  trait Matter
  trait Solid       extends Matter
  case class Rock() extends Solid  {
    override def toString: String = "#"
  }
  case class Air()  extends Matter {
    override def toString: String = "."
  }
  case class Sand() extends Solid  {
    override def toString: String = "o"
  }

  case class Source() extends Matter {
    override def toString: String = "+"
  }

  type Cave = Map[Coord, Matter]

  val start = Coord(500, 0)
  val cave  = Map[Coord, Matter](start -> Source())

  val rocks = xxx.foldLeft(cave) { case (cave, rock) =>
    val rockPaths: List[Coord] =
      rock
        .split("->")
        .map(_.trim)
        .map(_.split(",").map(_.toInt).toList)
        .map { case i0 :: i1 :: Nil => Coord(i0, i1) }
        .toList

    val allCoords = rockPaths
      .sliding(2)
      .flatMap { case Coord(r0x, r0y) :: Coord(r1x, r1y) :: Nil =>
        val (xS, xE)           = if (r0x <= r1x) (r0x, r1x) else (r1x, r0x)
        val (yS, yE)           = if (r0y <= r1y) (r0y, r1y) else (r1y, r0y)
        val coords: Seq[Coord] = for {
          x <- Range.inclusive(xS, xE)
          y <- Range.inclusive(yS, yE)
        } yield Coord(x, y)
        coords
      }
      .toList

    allCoords.distinct.foldLeft(cave) { case (c, coord) => c.updated(coord, Rock()) }
  }

  println(rocks)

  def printCave(cave: Cave): Unit = {
    val xRangeMin = cave.keys.minBy(_.x).x
    val xRangeMax = cave.keys.maxBy(_.x).x

    val yRangeMin = cave.keys.minBy(_.y).y
    val yRangeMax = cave.keys.maxBy(_.y).y
    val rows      = Range
      .inclusive(yRangeMin, yRangeMax + 2)
      .map(y => Range.inclusive(xRangeMin - 2, xRangeMax + 2).map(x => cave.getOrElse(Coord(x, y), Air())))
    rows.foreach(row => println(row.mkString("")))
    println("")
  }

  @tailrec
  def moveSand(cave: Cave, position: Coord): Cave = {
    val whatsBelow = cave.keys.filter(_.x == position.x).filterNot(_.y <= position.y).toList
    whatsBelow match {
      case Nil            => cave
      case somethingBelow =>
        val firstHit = somethingBelow.minBy(_.y)
        val below    = cave.getOrElse(firstHit, Air())
        val dl       = cave.getOrElse(firstHit.above.downLeft, Air())
        val dr       = cave.getOrElse(firstHit.above.downRight, Air())

        (below, dl, dr) match {
          case (_: Solid, _: Solid, _: Solid) => cave.updated(firstHit.above, Sand())
          case (_: Solid, _: Air, _)          => moveSand(cave, firstHit.above.downLeft)
          case (_: Solid, _: Solid, _: Air)   => moveSand(cave, firstHit.above.downRight)
        }
    }
  }

  @tailrec
  def run(cave: Cave, start: Coord = start, sand: Int = 0): (Cave, Int) = {
    val moved = moveSand(cave, start)
    if (moved == cave)
      (cave, sand)
    else
      run(moved, start, sand + 1)
  }

  println("p1")
//  val t0          = System.currentTimeMillis()
//  val (end, sand) = run(rocks)
//  printCave(end)
//  println(System.currentTimeMillis() - t0)
//  println(sand)

  println("p2")

  @tailrec
  def moveSand2(cave: Cave, position: Coord, lowestY: Int): Cave = {

    def getWithFloor(cave: Cave, pos: Coord, floor: Int): Matter = {
      if (pos.y == floor) Rock()
      else cave.getOrElse(pos, Air())
    }

    val thereIsAlwaysSomethingBelow = cave.keys.filter(_.x == position.x).filterNot(_.y <= position.y).toList match {
      case Nil           => Coord(position.x, lowestY)
      case alreadyExists => alreadyExists.minBy(_.y)
    }

    val below = getWithFloor(cave, thereIsAlwaysSomethingBelow, lowestY)
    val dl    = getWithFloor(cave, thereIsAlwaysSomethingBelow.above.downLeft, lowestY)
    val dr    = getWithFloor(cave, thereIsAlwaysSomethingBelow.above.downRight, lowestY)

    (below, dl, dr) match {
      case (_: Solid, _: Solid, _: Solid) => cave.updated(thereIsAlwaysSomethingBelow.above, Sand())
      case (_: Solid, _: Air, _)          => moveSand2(cave, thereIsAlwaysSomethingBelow.above.downLeft, lowestY)
      case (_: Solid, _: Solid, _: Air)   => moveSand2(cave, thereIsAlwaysSomethingBelow.above.downRight, lowestY)
    }
  }

  @tailrec
  def run2(cave: Cave, lowestY: Int, start: Coord = start, sand: Int = 0): (Cave, Int) = {
    val moved = moveSand2(cave, start, lowestY)
//    printCave(moved)
    if (moved(start) == Sand())
      (moved, sand + 1)
    else
      run2(moved, lowestY, start, sand + 1)
  }

  val t1            = System.currentTimeMillis()
  val (end2, sand2) = run2(rocks, rocks.keys.maxBy(_.y).y + 2)
  printCave(end2)
  println(System.currentTimeMillis() - t1)
  println(sand2)

}
