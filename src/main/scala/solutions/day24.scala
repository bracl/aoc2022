package solutions

import utils.IO._

object day24 extends App {

  val ex  = readExample(24)
  println(ex)
  val xxx = readFile(24)
  println(xxx)

  case class Coord(x: Int, y: Int) {
    def add(c: Coord): Coord = Coord(x + c.x, y + c.y)

    def move(d: Blizzard, maxX: Int, maxY: Int): Coord = {
      val proposed = d match {
        case Right() => Coord(x + 1, y)
        case Left()  => Coord(x - 1, y)
        case Up()    => Coord(x, y - 1)
        case Down()  => Coord(x, y + 1)
      }
      if (proposed.x > maxX) Coord(1, proposed.y)
      else if (proposed.x < 1) Coord(maxX, proposed.y)
      else if (proposed.y < 1) Coord(proposed.x, maxY)
      else if (proposed.y > maxY) Coord(proposed.x, 1)
      else proposed
    }
  }

  trait Blizzard

//  case class E() extends Square {
//    override def toString: String = "E"
//  }

  case class Left()  extends Blizzard {
    override def toString: String = "<"
  }
  case class Right() extends Blizzard {
    override def toString: String = ">"
  }
  case class Up()    extends Blizzard {
    override def toString: String = "^"
  }
  case class Down()  extends Blizzard {
    override def toString: String = "v"
  }

  type BMap   = Map[Coord, List[Blizzard]]
  type Search = Vector[(Int, Valley)]

  case class Valley(blizzards: BMap, pos: Coord, maxX: Int, maxY: Int, endState: Coord) {

    def finished(): Boolean = pos == endState

    def printValley(party: Set[Coord]): Unit = {
      val keys = blizzards.keys.toList
      val maxX = keys.maxBy(_.x).x
      val minX = keys.minBy(_.x).x
      val minY = keys.minBy(_.y).y
      val maxY = keys.maxBy(_.y).y

      Range.inclusive(minY, maxY).foreach { y =>
        println(
          Range
            .inclusive(minX, maxX)
            .map { x =>
              if (party.contains(Coord(x, y))) "E"
              else {
                blizzards.get(Coord(x, y)) match {
                  case None                    => " "
                  case Some(Nil)               => "."
                  case Some(l) if l.length > 1 => l.length.toString
                  case Some(l)                 => l.head.toString
                }
              }
            }
            .mkString(""),
        )
      }

    }

    def updateBlizzards(): Valley = {
      val s: BMap = blizzards.toList.map { case (k, _) =>
        (k, Nil)
      }.toMap
      val updated = blizzards.toList.foldLeft(s) { case (bmap, (k, v)) =>
        val updates: List[(Coord, Blizzard)] = v.map(bl => (k.move(bl, this.maxX, this.maxY), bl))
        updates.foldLeft(bmap) { case (bbb, (c, v)) =>
          bbb.updated(c, bbb.getOrElse(c, Nil).appended(v))
        }
      }
      this.copy(blizzards = updated)
    }

    def moveParty(): List[Coord] = {
      val adjacentSquares = List(Coord(0, 0), Coord(1, 0), Coord(0, 1), Coord(-1, 0), Coord(0, -1)).map(c => pos.add(c))
      val availableMoves  = adjacentSquares.filter(c => blizzards.contains(c) && blizzards(c) == Nil)
      availableMoves
    }
  }

  def read(lines: List[String]): Valley = {
    val m = lines.zipWithIndex.foldLeft(Map[Coord, List[Blizzard]]()) { case (mapp, (l, y)) =>
      l.zipWithIndex.foldLeft(mapp) { case (mappp, (c, x)) =>
        c match {
          case '#' => mappp
          case '.' => mappp.updated(Coord(x, y), Nil)
          case '>' => mappp.updated(Coord(x, y), List(Right()))
          case '<' => mappp.updated(Coord(x, y), List(Left()))
          case '^' => mappp.updated(Coord(x, y), List(Up()))
          case 'v' => mappp.updated(Coord(x, y), List(Down()))
        }
      }
    }
    Valley(m, m.keys.minBy(_.y), lines.head.length - 2, lines.length - 2, m.keys.maxBy(_.y))
  }

  def positions(minute: Int, coords: Set[Coord], valley: Valley, addedOn: Int = 0): Int = {
    println(minute)
    if (coords.contains(valley.endState)) {
      if (addedOn == 0) {
        println("got there once -> heading back")
        val valley2 = valley.copy(pos = valley.endState, endState = valley.blizzards.keys.minBy(_.y))
        goBack(minute, 0, Set(valley.endState), valley2)
      } else addedOn + minute
    } else {
      val blizz        = valley.updateBlizzards()
      val valleys      = coords.map(c => blizz.copy(pos = c))
      val newPositions = valleys.flatMap(_.moveParty())
//      blizz.printValley(newPositions)
      assert(newPositions.forall(c => blizz.blizzards(c) == Nil))
      positions(minute + 1, newPositions, blizz, addedOn)
    }
  }

  def goBack(gotThereIn: Int, minute: Int, coords: Set[Coord], valley: Valley): Int = {
    println(minute)
    if (coords.contains(valley.endState)) {
      println("got back to start => going forward")
      val valley3 = valley.copy(pos = valley.endState, endState = valley.blizzards.keys.maxBy(_.y))
      positions(0, Set(valley.endState), valley3, gotThereIn + minute)
    } else {
      val blizz        = valley.updateBlizzards()
      val valleys      = coords.map(c => blizz.copy(pos = c))
      val newPositions = valleys.flatMap(_.moveParty())
      //      blizz.printValley(newPositions)
      assert(newPositions.forall(c => blizz.blizzards(c) == Nil))
      goBack(gotThereIn, minute + 1, newPositions, blizz)
    }
  }

  val valley = read(xxx)
//  Range.inclusive(1, 18).foldLeft(valley) { case (v, i) =>
//    val u = v.updateBlizzards()
//    println(i)
//    u.printValley()
//    u
//  }
  println("p1")

  val p1 = positions(0, Set(valley.pos), valley)
  println(p1)

  println("p2")

}
