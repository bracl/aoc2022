package solutions

import utils.Coord
import utils.IO.*

import scala.annotation.tailrec

object day17 extends App {

  val ex  = readExample(17)
  println(ex)
  val xxx = readFile(17)
  println(xxx)

  type Gas = String

  trait Matter

  case class Air()  extends Matter {
    override def toString: String = "."
  }
  case class Wall() extends Matter

  case class FallenRock() extends Matter {
    override def toString: String = "#"
  }

  trait Rock                                                                                      extends Matter {
    def add(c: Coord): Rock
    def down: Rock  = this.add(Coord(0, -1))
    def left: Rock  = this.add(Coord(-1, 0))
    def right: Rock = this.add(Coord(0, 1))
    def coords: Set[Coord]
  }
  case class Line(coords: Set[Coord] = Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0)))   extends Rock   {
    def add(c: Coord): Line = this.copy(coords = this.coords.map(r => r + c))
    override def toString   = "line"
  }
  case class Plus(coords: Set[Coord] = Set(Coord(0, 1), Coord(1, 0), Coord(1, 1), Coord(1, 2), Coord(2, 1)))
      extends Rock {
    def add(c: Coord): Plus = this.copy(coords = this.coords.map(r => r + c))
    override def toString   = "plus"
  }
  case class Ell(coords: Set[Coord] = Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(2, 1), Coord(2, 2)))
      extends Rock {
    def add(c: Coord): Ell = this.copy(coords = this.coords.map(r => r + c))
    override def toString  = "ell"
  }
  case class Pipe(coords: Set[Coord] = Set(Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3)))   extends Rock   {
    def add(c: Coord): Pipe = this.copy(coords = this.coords.map(r => r + c))
    override def toString   = "pipe"
  }
  case class Square(coords: Set[Coord] = Set(Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1))) extends Rock   {
    def add(c: Coord): Square = this.copy(coords = this.coords.map(r => r + c))
    override def toString     = "square"
  }

  case class Cave(gas: Gas, m: Set[Coord] = Set(), rocks: Long = 0) {
    def get(c: Coord): Matter = {
      if (c.y < 0) Wall()
      else if (c.x < 0 || c.x > 6) Wall()
      else if (m.contains(c)) FallenRock()
      else Air()
    }

    def highestRock: Long = m match {
      case s if s.isEmpty => -1
      case s              => s.maxBy(_.y).y
    }

    def settleRock(r: Rock): Cave = this.copy(m = this.m ++ r.coords, rocks = this.rocks + 1)

    def collision(rock: Rock): Boolean = rock.coords.exists(c => this.get(c) != Air())

    def moveUnlessHit(r: Rock, direction: Char): Rock = {
      val dir = direction match {
        case '>' => Coord(1, 0)
        case '<' => Coord(-1, 0)
      }

      def move(r: Rock, d: Coord): Rock = {
        val moved     = r.add(d)
        val collision = this.collision(moved)
        if (collision) r
        else moved
      }

      val maybeHit = move(r, dir)
      maybeHit
    }

  }

  def dropRock(cave: Cave, rock: Rock, gas: Gas): (Cave, Gas) = {
    val starting = rock.add(Coord(2, 0)).add(Coord(0, cave.highestRock + 4))

    @tailrec
    def moveRock(cave: Cave, rock: Rock, gas: Gas, down: Boolean): (Cave, Rock, Gas, Boolean, Boolean) = {
      val (c, r, g, d, settled) = if (down) {
        val candidateMove = rock.down
        val hitSomething  = cave.collision(candidateMove)
        if (debug) println(hitSomething)
        if (hitSomething) {
          // place the rock in the cave, set down to be false, settled to be true
          // rock and gas are semi-irrelevant
          (cave.settleRock(rock), rock, gas, false, true)
        } else {
          // cave remains the same, drop the rock, down is false, settled is false
          // gas in unchanged
          (cave, candidateMove, gas, false, false)
        }
      } else {
        val direction: Char = gas.head
        if (debug) println(direction)
        // rock cannot settle on a gas move
        // cave stays the same, one gas is used, down is now true, settled is false
        // rock is updated to move as far in the direction as possible
        val shifted         = cave.moveUnlessHit(rock, direction)
        if (debug) println(shifted)

        val remainingGas = gas.tail match {
          case "" => cave.gas
          case g  => g
        }
        (cave, shifted, remainingGas, true, false)
      }
      if (settled) (c, r, g, d, settled)
      else moveRock(c, r, g, d)
    }

    val (c, _, g, _, _) = moveRock(cave, starting, gas, down = false)
    (c, g)
  }

  def dropLines(c: Cave): Cave = {
    val x  = 50
    val hr = c.highestRock
    if (hr > x) c.copy(m = c.m.filterNot(c => c.y < hr - x))
    else c
  }

  val gas   = xxx.head
  val rocks = List(Line(), Plus(), Ell(), Pipe(), Square())

  val debug = false

  val ugh = 1000000000000L

  val lazyL = LazyList.continually(0)
  val rock  = 1L

  type FloorConfig = String
  type GRMM        = Map[(FloorConfig, Gas), (Cave, Long, Long)]
  val gasRockMemoryMap: GRMM = Map()

  def floorState(cave: Cave, rock: Rock): FloorConfig = Range
    .inclusive(0, 50)
    .map(_.toLong)
    .map(i => cave.highestRock - i)
    .map(y => Range.inclusive(0, 6).map(x => cave.get(Coord(x, y))).mkString(""))
    .mkString("") + rock.toString

  val added           = (0L, 0L)
  val (cave, _, _, _) = lazyL.foldLeft((Cave(gas), gas, gasRockMemoryMap, added)) { case ((cave, gas, grmm, a), _) =>
    val r                      = cave.rocks + a._1
    println(r)
    val rock                   = rocks((r % rocks.length).toInt)
    val (rested, remainingGas) = dropRock(cave, rock, gas)
    val dropped                = if (r % 10 == 0) dropLines(rested) else rested

    val fs  = floorState(dropped, rock)
    val key = (fs, remainingGas)

    def applyPayload(cave: Cave, rI: Long, hI: Long): (Long, Long) = {
      val couldApply: Long = Math.floor((ugh - cave.rocks) / rI).toLong
      (rI * couldApply, hI * couldApply)
//      cave.copy(rocks = cave.rocks + (rI * couldApply), m = cave.m.map(c => c.add(Coord(0, (hI * couldApply)))))
    }

    def calculateDiff(grmm: GRMM, k: (FloorConfig, Gas), c0: Cave, c1: Cave): GRMM = {
      val rockInc   = c1.rocks - c0.rocks
      val heightInc = c1.highestRock - c0.highestRock
      grmm.updated(k, (c1, rockInc, heightInc))
    }

    val (newGrmm, newCave, applied): (GRMM, Cave, (Long, Long)) = if (!grmm.contains(key)) {
      val g: GRMM = grmm.updated(key, (dropped, 0, 0))
      (g, dropped, a)
    } else {
      val (previousCave, rockInc, heightInc) = grmm(key)
      if (rockInc == 0 && heightInc == 0) {
        val updatedGrmm: GRMM = calculateDiff(grmm, key, previousCave, dropped)
        val (_, ri, hi)       = updatedGrmm(key)

        if (ugh > r + ri)
          val applied = applyPayload(dropped, ri, hi)
          (updatedGrmm, dropped, applied)
        else
          val filtered = updatedGrmm.filterNot(_._2._2 > ugh - dropped.rocks)
          (filtered, dropped, a)
      } else {
        if (ugh > r + rockInc)
          val applied = applyPayload(dropped, rockInc, heightInc)
          (grmm, dropped, applied)
        else (grmm, dropped, a)
      }
    }

    if (r == ugh)
      val height: Long = cave.highestRock + 1
      val withRocks    = height + applied._2
      println(withRocks)
      System.exit(0)

    (newCave, remainingGas, newGrmm, applied)
  }

}
