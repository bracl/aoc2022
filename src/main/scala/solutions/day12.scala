package solutions

import utils.Coord
import utils.Coord.given_Conversion_Long_Int
import utils.IO._

object day12 extends App {

  val xxx = readFile(12).map(_.toVector).toVector

  println(xxx)
//  case class Coord(x: Int, y: Int) {
//    def +(c: Coord): Coord = this.copy(x + c.x, y + c.y)
//    def onMap: Boolean     = x >= 0 && y >= 0 && x < xxx.head.length && y < xxx.length
//  }
  val directions = List(Coord(1, 0), Coord(-1, 0), Coord(0, 1), Coord(0, -1))
  type Path        = Vector[Coord]
  type DistanceMap = Vector[Vector[Int]]
  case class Map(m: Vector[Vector[Char]]) {
    def position(char: Char): Coord = {
      val row = m.indexWhere(_.contains(char))
      val col = m(row).indexWhere(_ == char)
      assert(m(row)(col) == char)
      Coord(col, row)
    }

    def getHeight(c: Coord): Char = m(c.y)(c.x)

    def printPath(p: Path): Unit = {
      println(p.map(getHeight).mkString(""))
    }
  }

  def charToInt(c: Char): Int = {
    c match {
      case lower if lower.isLower => lower.toInt - 96
      case 'S'                    => 1
      case 'E'                    => 26
      case _                      => throw new RuntimeException("Whoops")
    }
  }

  def ascending(c0: Char, c1: Char): Boolean  = charToInt(c1) <= charToInt(c0) + 1
  def descending(c0: Char, c1: Char): Boolean = charToInt(c1) >= charToInt(c0) - 1

  def moves(m: Map, path: Path, allowed: (Char, Char) => Boolean): Seq[Path] = {
    val current                 = path.last
    val previous: Option[Coord] = if (path.length > 1) Some(path(path.length - 2)) else None
    val height                  = m.getHeight(current)
    val surrounding             = directions
      .map(dir => current + dir)
      .filter(_.withinBox(0, xxx.head.length - 1, 0, xxx.length - 1))
      .filterNot(c => previous.contains(c))
      .map(c => (c, m.getHeight(c)))
    val moveable                = surrounding.filter { case (_, h) => allowed(height, h) }
    moveable.map { case (c, _) =>
      path.appended(c)
    }
  }

  def reducePaths(paths: Set[Path]): Set[Path] = {
    val grouped = paths.groupBy(_.last)
    val minPath = grouped.map { case (_, paths) => paths.minBy(_.size) }.toSet
    val noLoops = minPath.filter { p =>
      !p.dropRight(1).contains(p.last)
    }
    noLoops
  }

  def updateDistanceMap(paths: Set[Path], d: DistanceMap): (DistanceMap, Set[Path]) = {
    paths.foldLeft((d, Set[Path]())) { case ((distanceMap: DistanceMap, acc: Set[Path]), path) =>
      val last           = path.last
      val shortestToLast = distanceMap(last.y)(last.x)
      shortestToLast match {
        case x if x < path.length - 1 => (distanceMap, acc)
        case _                        =>
          val upd: Vector[Vector[Int]] =
            distanceMap.updated(last.y, distanceMap(last.y).updated(last.x, path.length - 1))
          (upd, acc + path)
      }
    }
  }

  def solve(
    m: Map,
    d: Vector[Vector[Int]],
    paths: Set[Path],
    moveable: (Char, Char) => Boolean,
    reduce: Set[Path] => Set[Path],
    endCondition: (Path, Map) => Boolean,
    endPaths: Set[Path] = Set(),
  ): Set[Path] = {
    val moved: Set[Path] = paths.flatMap { path =>
      moves(m, path, moveable)
    }

    val reduced         = reduce(moved)
    val (upD, shortest) = updateDistanceMap(reduced, d)

    val (ended, stillGoing) = shortest.partition(p => endCondition(p, m))
    stillGoing match {
      case s if s.isEmpty => endPaths ++ ended
      case s              =>
        solve(m, upD, s, moveable, reduce, endCondition, endPaths ++ ended)
    }

  }

  val m                                    = Map(xxx)
  val start                                = Vector(m.position('S'))
  val distanceMapAsc: Vector[Vector[Int]]  = start.map { s =>
    val m = xxx.map(_.map(_ => 9999))
    m.updated(s.y, m(s.y).updated(s.x, 0))
  }.head
  val end                                  = m.position('E')
  val distanceMapDesc: Vector[Vector[Int]] = Vector(end).map { e =>
    val m = xxx.map(_.map(_ => 9999))
    m.updated(e.y, m(e.y).updated(e.x, 0))
  }.head
  def highest(p: Path, m: Map)             = p.last == end
  val allPaths                             = solve(m, distanceMapAsc, Set(start), ascending, reducePaths, highest)
  println(allPaths.filter(p => p.contains(end)).map(_.length).min - 1)

  def isA(p: Path, m: Map) = charToInt(m.getHeight(p.last)) == 1
  val anyA                 = solve(m, distanceMapDesc, Set(Vector(end)), descending, reducePaths, isA)
  println(anyA.minBy(_.length).length - 1)
}
