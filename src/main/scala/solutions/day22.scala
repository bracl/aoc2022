package solutions

import utils.IO.*

import scala.annotation.tailrec

object day22 extends App {

  val ex  = readExample(22, false)
  val xxx = readFile(22, false)
  println(ex)
  println(xxx)

  type Cave = Map[Coord, Matter]

  def printCave(c: Map[Coord, Any]): Unit = {
    val maxX = c.keys.maxBy(_.x).x
    val maxY = c.keys.maxBy(_.y).y
    Range.inclusive(1, maxY).foreach { y =>
      println(
        Range
          .inclusive(1, maxX)
          .map { x =>
            c.getOrElse(Coord(x, y), Air())
          }
          .mkString(""),
      )
    }
  }

  def isCave(c: Coord, cave: Cave): Boolean = cave.contains(c)

  case class Coord(x: Int, y: Int) {
    override def toString: String              = s"($x, $y)"
    def add(c: Coord): Coord                   = Coord(x + c.x, y + c.y)
    def addOrWrap(c: Coord, cave: Cave): Coord = {
      val moved = this.add(c)
      if (isCave(moved, cave)) moved
      else {
        c match {
          case Coord(1, 0)  => cave.keys.filter(_.y == this.y).minBy(_.x)
          case Coord(0, 1)  => cave.keys.filter(_.x == this.x).minBy(_.y)
          case Coord(-1, 0) => cave.keys.filter(_.y == this.y).maxBy(_.x)
          case Coord(0, -1) => cave.keys.filter(_.x == this.x).maxBy(_.y)
        }
      }
    }

    def face: Int = if (x > 50 && x <= 100 && y <= 50) { 1 }
    else if (x > 100 && y <= 50) { 2 }
    else if (x > 50 && x <= 100 && y > 50 && y <= 100) { 3 }
    else if (x > 50 && x <= 100 && y > 100 && y <= 150) { 6 }
    else if (x <= 50 && y > 100 && y <= 150) { 5 }
    else if (x <= 50 && y > 150) { 4 }
    else throw new RuntimeException("flange")

    def addOrCube(c: Coord, cave: Cave, dir: Int): (Coord, Int) = {
      val moved = this.add(c)
      if (isCave(moved, cave)) (moved, dir)
      else {
        (this.face, c) match {
          // (51, 1) => (1, 50+x)
          // on 1 and moving up
          case (1, Coord(-1, 0)) => (Coord(1, 151 - this.y), 0)
          // (51, 1) => (1, 100+x)
          // on 1 and moving left
          case (1, Coord(0, -1)) => (Coord(1, 100 + this.x), 0)

          // on 2 and moving right
          // (150,50) => (100, 101)
          // (150, 1) => (100, 150)
          case (2, Coord(1, 0))  => (Coord(100, 151 - this.y), 180)
          // on 2 and moving up
          // (101,1) => (1,200)
          // (150, 1) => (50,200)
          case (2, Coord(0, -1)) => (Coord(this.x - 100, 200), 270)
          // on 2 and moving down
          // (101,50) => (100,51)
          // (150, 50) => (100,100)
          case (2, Coord(0, 1))  => (Coord(100, this.x - 50), 180)

          // on 3 and moving right
          // (100,51) => (101,50)
          // (100,100) => (150, 50)
          case (3, Coord(1, 0))  => (Coord(this.y + 50, 50), 270)
          // on 3 and moving left
          // (51,51) => (1,101)
          // (51,100) => (50,101)
          case (3, Coord(-1, 0)) => (Coord(this.y - 50, 101), 90)

          // on 4 and moving left
          // (1,151) => (51,1)
          // (1,200) => (100,1)
          case (4, Coord(-1, 0)) => (Coord(this.y - 100, 1), 90)
          // on 4 and moving right
          // (50,151) => (51,150)
          // (50,200) => (100,150)
          case (4, Coord(1, 0))  => (Coord(this.y - 100, 150), 270)
          // on 4 and moving down
          // (1,200) => (101,1)
          // (50,200) => (150,1)
          case (4, Coord(0, 1))  => (Coord(this.x + 100, 1), 90)

          // on 5 and moving up
          // (1,101) => (51,51)
          // (50,101) => (51,100)
          case (5, Coord(0, -1)) => (Coord(51, this.x + 50), 0)
          // on 5 and moving left
          // (1,101) => (51,50)
          // (1,150) => (51,1)
          case (5, Coord(-1, 0)) => (Coord(51, 151 - this.y), 0)

          // on 6 and moving right
          // (100,101) => (150,50)
          // (100,150) => (150,1)
          case (6, Coord(1, 0)) => (Coord(150, 151 - this.y), 180)
          // on 6 and moving down
          // (51,150) => (50,151)
          // (100,150) => (50,200)
          case (6, Coord(0, 1)) => (Coord(50, this.x + 100), 180)
        }
      }
    }
  }

  trait Matter
  trait Direction    extends Matter
  case class Wall()  extends Matter    {
    override def toString: String = "#"
  }
  case class Floor() extends Matter    {
    override def toString: String = "."
  }
  case class Air()   extends Matter    {
    override def toString: String = " "
  }
  case class Up()    extends Direction {
    override def toString: String = "^"
  }
  case class Down()  extends Direction {
    override def toString: String = "v"
  }
  case class Right() extends Direction {
    override def toString: String = ">"
  }
  case class Left()  extends Direction {
    override def toString: String = "<"
  }

  def buildMap(l: List[String]): Cave = {
    val start: Cave = Map()
    l.zipWithIndex.foldLeft(start) { case (s, (line, rowMinusOne)) =>
      val whiteSpace = line.takeWhile(_.isWhitespace).length
      val remaining  = line.drop(whiteSpace).filterNot(_.isWhitespace)
      val coords     = remaining.zipWithIndex.map { (c, i) =>
        c match {
          case '.' => Coord(whiteSpace + i + 1, rowMinusOne + 1) -> Floor()
          case '#' => Coord(whiteSpace + i + 1, rowMinusOne + 1) -> Wall()
          case _   => throw new RuntimeException("tits")
        }
      }
      val u          = coords.foldLeft(s) { case (m, (c, matter)) => m.updated(c, matter) }
      u
    }
  }

  @tailrec
  def prepInstructions(str: String, acc: List[String] = Nil): List[String] = {
    str.headOption match {
      case None      => acc
      case Some('L') => prepInstructions(str.tail, acc :+ "L")
      case Some('R') => prepInstructions(str.tail, acc :+ "R")
      case Some(_)   =>
        val amt = str.takeWhile(_.isDigit).mkString("")
        prepInstructions(str.drop(amt.length), acc :+ amt)
    }
  }

  def startPosition(m: Cave): Coord = m.keys.filter(_.y == 1).minBy(_.x)

  def scoreDirection(i: Int): Int = {
    i match {
      case 0   => 0
      case 90  => 1
      case 180 => 2
      case 270 => 3
    }
  }

  def score(c: Coord, d: Int): Int = c.y * 1000 + c.x * 4 + scoreDirection(d)

  def directionToMatter(i: Int): Matter = i match {
    case 0   => Right()
    case 90  => Down()
    case 180 => Left()
    case 270 => Up()
  }

  def goodState(
    c: Cave,
    previous: Coord,
    candidate: Coord,
    previousDir: Int,
    candidateDir: Int,
  ): (Cave, Boolean, Coord, Int) =
    c.getOrElse(candidate, Air()) match {
      case Wall()       =>
        val u = c.updated(previous, directionToMatter(previousDir))
        (u, true, previous, previousDir)
      case _: Direction =>
        val u = c.updated(candidate, directionToMatter(candidateDir))
        (u, false, candidate, candidateDir)
      case Floor()      =>
        val u = c.updated(candidate, directionToMatter(candidateDir))
        (u, false, candidate, candidateDir)
      case Air()        => throw new RuntimeException("fuck")
    }

  def move(cave: Cave, pos: Coord, dir: Int, amt: Int): (Cave, Coord, Int) = {
    val (updatedCave, _, end) = Range.inclusive(1, amt).foldLeft((cave, false, pos)) { case ((c, stopped, p), _) =>
      if (stopped) (c, stopped, p)
      else {
        val candidate                        = dir match {
          case 0   => p.addOrWrap(Coord(1, 0), cave)
          case 90  => p.addOrWrap(Coord(0, 1), cave)
          case 180 => p.addOrWrap(Coord(-1, 0), cave)
          case 270 => p.addOrWrap(Coord(0, -1), cave)
        }
        val (updated, isWall, maybeMoved, _) = goodState(c, p, candidate, dir, dir)
        (updated, isWall, maybeMoved)
      }
    }
    (updatedCave, end, dir)
  }

  def moveCube(cave: Cave, pos: Coord, dir: Int, amt: Int): (Cave, Coord, Int) = {
    val (updatedCave, _, end, endDir) =
      Range.inclusive(1, amt).foldLeft((cave, false, pos, dir)) { case ((c, stopped, p, d), _) =>
        if (stopped) (c, stopped, p, d)
        else {
          val (candidate, candidateDir)                            = d match {
            case 0   => p.addOrCube(Coord(1, 0), cave, d)
            case 90  => p.addOrCube(Coord(0, 1), cave, d)
            case 180 => p.addOrCube(Coord(-1, 0), cave, d)
            case 270 => p.addOrCube(Coord(0, -1), cave, d)
          }
          val (updatedCave, isWall, maybeMovedPosition, direction) = goodState(c, p, candidate, d, candidateDir)
          (updatedCave, isWall, maybeMovedPosition, direction)
        }
      }
    (updatedCave, end, endDir)
  }

  def process(ins: List[String], c: Cave, moveFn: (Cave, Coord, Int, Int) => (Cave, Coord, Int)) = {
    val start     = startPosition(running)
    val direction = 0
    val path      = Vector[(Coord, Int)]()
    ins.foldLeft((c, start, direction, path)) { case ((cave, pos, dir, path), i) =>
      i match {
        case "L"                      =>
          val i1 = (dir - 90 + 360) % 360
          val u  = cave.updated(pos, directionToMatter(i1))
          (u, pos, i1, path)
        case "R"                      =>
          val i1 = (dir + 90) % 360
          val u  = cave.updated(pos, directionToMatter(i1))
          (u, pos, i1, path)
        case x if x.forall(_.isDigit) =>
          val (c, p, d) = moveFn(cave, pos, dir, x.toInt)
          (c, p, d, path.appended((p, d / 90)))
      }
    }
  }

  lazy val exMap                         = buildMap(ex.takeWhile(s => s != ""))
  lazy val exInstructions: List[String]  = prepInstructions(ex.last)
  lazy val xxxMap                        = buildMap(xxx.takeWhile(s => s != ""))
  lazy val xxxInstructions: List[String] = prepInstructions(xxx.last)

  val (running, instructions) = if (false) (exMap, exInstructions) else (xxxMap, xxxInstructions)

  val (_, ppp, ddd, _) = process(instructions, running, move)
  println(score(ppp, ddd))

  val (_, ppp2, ddd2, _) = process(instructions, running, moveCube)
  println(score(ppp2, ddd2))

}
