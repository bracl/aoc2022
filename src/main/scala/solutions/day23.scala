package solutions

import solutions.day23.start
import utils.IO.*

object day23 extends App {

  val ex  = readExample(23)
  println(ex)
  val xxx = readFile(23)
  println(xxx)

  val directions                         = List(
    "N",
    "S",
    "W",
    "E",
  )
  def nextOrder(d: List[Int]): List[Int] = d.tail :+ d.head

  def printElves(elves: Set[Elf]): Unit = {
    val maxX = elves.maxBy(_.x).x
    val minX = elves.minBy(_.x).x
    val minY = elves.minBy(_.y).y
    val maxY = elves.maxBy(_.y).y

    Range.inclusive(minY - 1, maxY + 1).reverse.foreach { y =>
      println(
        Range
          .inclusive(minX - 1, maxX + 1)
          .map {
            case x if elves.contains(Elf(x, y)) => '#'
            case _                              => '.'
          }
          .mkString(""),
      )
    }
  }

  case class Elf(x: Int, y: Int) {
    def add(c: Elf): Elf   = Elf(x + c.x, y + c.y)
    def adjacent: Set[Elf] = {
      (for {
        x <- List(-1, 0, 1)
        y <- List(-1, 0, 1)
      } yield this.add(Elf(x, y))).filterNot(_ == this).toSet
    }

    def adjacentElves(elves: Set[Elf]): Set[Elf] = this.adjacent.intersect(elves)

    def lookForElf(elves: Set[Elf], dIndex: Int): Boolean = {
      val candidates: Set[Elf] = dIndex match {
        case 0 => Set(-1, 0, 1).map(i => this.add(Elf(i, 1)))
        case 1 => Set(-1, 0, 1).map(i => this.add(Elf(i, -1)))
        case 2 => Set(-1, 0, 1).map(i => this.add(Elf(-1, i)))
        case 3 => Set(-1, 0, 1).map(i => this.add(Elf(1, i)))
        case _ => throw new RuntimeException("bollox")
      }
      candidates.intersect(elves).nonEmpty
    }

    def isAlone(elves: Set[Elf]): Boolean = this.adjacentElves(elves).isEmpty

    def proposeMove(order: List[Int], elves: Set[Elf]): Elf = {
      val direction = order.dropWhile(d => this.lookForElf(this.adjacentElves(elves), d)).headOption
      direction
        .map(d =>
          directions(d) match {
            case "N" => this.add(Elf(0, 1))
            case "S" => this.add(Elf(0, -1))
            case "W" => this.add(Elf(-1, 0))
            case "E" => this.add(Elf(1, 0))
          },
        )
        .getOrElse(this)
    }
  }

  def score(elves: Set[Elf]): Int = {
    val maxX = elves.maxBy(_.x).x
    val minX = elves.minBy(_.x).x
    val minY = elves.minBy(_.y).y
    val maxY = elves.maxBy(_.y).y

    val x     = Range.inclusive(minX, maxX).length
    val y     = Range.inclusive(minY, maxY).length
    val box   = x * y
    val space = box - elves.size
    space
  }

  def processRound(elves: Set[Elf], order: List[Int]): (Set[Elf], List[Int]) = {
    val (dontDoAnything, doSomething)   = elves.partition(e => e.isAlone(elves))
    val proposedMoves: List[(Elf, Elf)] = doSomething.toList.map(e => (e, e.proposeMove(order, elves)))
    val (actual, nonMovers)             = proposedMoves.groupBy(_._2).partition { case (_, v) => v.length == 1 }

    val triedToMoveToTheSamePlace: Set[Elf] = nonMovers.flatMap { case (_, v) => v.map(_._1) }.toSet
    val state: Set[Elf]                     = dontDoAnything ++ actual.keys.toSet ++ triedToMoveToTheSamePlace

    assert(elves.size == state.size)
    (state, nextOrder(order))
  }

  def simulate(turns: Int, startingState: Set[Elf]): Set[Elf] = {
    val (end, _) = Range.inclusive(1, turns).foldLeft((startingState, List(0, 1, 2, 3))) { case ((elves, order), i) =>
      println(s"$i")
      processRound(elves, order)
    }
    end
  }

  def untilStatic(turn: Int, start: Set[Elf], order: List[Int] = List(0, 1, 2, 3)): Int = {
    println(turn)
    val (s, o) = processRound(start, order)
    if (s == start) turn
    else untilStatic(turn + 1, s, o)
  }

  println("p1")
  val in    = xxx
  val start = in.zipWithIndex.flatMap { (l, i) =>
    l.zipWithIndex.flatMap {
      case ('#', j) => Some(Elf(j, in.length - 1 - i))
      case _        => None
    }
  }.toSet

  val end = simulate(10, start)
  val sc  = score(end)
  println(sc)

  println("p2")
  val turns = untilStatic(1, start)
  println(turns)
}
