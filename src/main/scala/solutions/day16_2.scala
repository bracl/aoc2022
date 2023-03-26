package solutions

import solutions.day12.{DistanceMap, updateDistanceMap}
import utils.IO.*

import scala.annotation.tailrec

object day16_2 extends App {

  val ex  = readExample(16)
  println(ex)
  val xxx = readFile(16)
  println(xxx)

  val minutes = 26

  case class Valve(name: String, on: Boolean, rate: Int, paths: List[String])

  type Cave = Map[String, Valve]
  case class Node(valve: Valve, arrived: Int) {
    override def toString: String = s"${valve.name}   $arrived"
  }
  type Path = (SinglePath, SinglePath)
  type SinglePath        = List[Node]
  type PressurePotential = Int
  type TimeToGetThere    = Int

  def read(lines: List[String]): Cave = {
    lines.foldLeft(Map[String, Valve]()) { case (m, s) =>
      val words = s.split(" ").toList
      val name  = words(1)
      val rate  = words.find(_.startsWith("rate")).get.split("=").last.filter(_.isDigit).toInt
      val on    = false
      val paths = words.reverse.map(_.replace(",", "")).takeWhile(_.forall(_.isUpper))
      val v     = Valve(name, on, rate, paths)
      m.updated(name, v)
    }
  }

  def shortestPath(start: String, system: Cave): Map[String, Int] = {
    val distanceMap = Map[String, Int](start -> 0)

    @tailrec
    def checkNodes(cave: Cave, distanceMap: Map[String, Int]): Map[String, Int] = {
      val updated = distanceMap.keys.foldLeft(distanceMap) { case (dM, n) =>
        val couldMove       = cave(n).paths
        val currentDistance = dM(n)
        couldMove.foldLeft(dM) { case (dM, cM) =>
          if (!dM.contains(cM)) dM.updated(cM, currentDistance + 1)
          else {
            val couldAlreadyGetThereIn = dM(cM)
            if (couldAlreadyGetThereIn > currentDistance + 1) dM.updated(cM, currentDistance + 1)
            else dM
          }
        }
      }
      if (updated == distanceMap) distanceMap
      else checkNodes(cave, updated)
    }
    checkNodes(system, distanceMap)
  }

  def potentialValue(
    pos: String,
    alreadyOpen: List[String],
    minute: Int,
    cave: Cave,
  ): Map[String, (PressurePotential, TimeToGetThere)] = {
    val distanceMap = shortestPath(pos, cave)
    val minutesLeft = minutes - minute
    val moves       = cave.filter { case (k, v) => k != pos && !alreadyOpen.contains(v.name) }.values
    val res: Map[String, (PressurePotential, TimeToGetThere)] = moves.map { valve =>
      val movesNeeded: TimeToGetThere = distanceMap(valve.name)
      val afterMoving                 = minutesLeft - movesNeeded
      val pressure: PressurePotential = math.max((afterMoving - 1) * valve.rate, 0)
      valve.name -> (pressure, movesNeeded)
    }.toMap
    //    println("\n")
    //    println(minute)
    //    res.foreach { case (k, v) => println(s"$k => $v") }
    res
  }

  def move(minute: Int, path: Path, cave: Cave): Set[Path] = {

    def addNodeToPath(path: SinglePath, v: Valve, distance: Int): SinglePath = {
      val node = Node(v, path.last.arrived + 1 + distance)
      path :+ node
    }

    val mine: Node     = path._1.last
    val elephant: Node = path._2.last

    val alreadyOpen: List[String] = (path._1.map(_.valve.name) ++ path._2.map(_.valve.name)).distinct

    val myPv       =
      if (mine.arrived < minute) potentialValue(mine.valve.name, alreadyOpen, mine.arrived, cave)
      else Map()
    val elephantPv =
      if (elephant.arrived < minute) potentialValue(elephant.valve.name, alreadyOpen, elephant.arrived, cave)
      else Map()

    val iCouldMove        = myPv.toList.sortBy(_._2).filterNot(_._2._1 == 0)
    val elephantCouldMove = elephantPv.toList.sortBy(_._2).filterNot(_._2._1 == 0)

    val myRes: Set[SinglePath]       = iCouldMove match {
      case Nil => Set()
      case x   => x.map(sM => addNodeToPath(path._1, cave(sM._1), sM._2._2)).toSet
    }
    val elephantRes: Set[SinglePath] = elephantCouldMove match {
      case Nil => Set()
      case x   => x.map(sM => addNodeToPath(path._2, cave(sM._1), sM._2._2)).toSet
    }

    val res = (myRes, elephantRes) match {
      case (m, e) if m.isEmpty && e.isEmpty   => Set(path)
      case (m, e) if m.isEmpty && e.nonEmpty  => e.map(p => (path._1, p))
      case (m, e) if m.nonEmpty && e.isEmpty  => m.map(p => (p, path._2))
      case (m, e) if m.nonEmpty && e.nonEmpty =>
        for {
          me  <- m
          ele <- e
        } yield (me, ele)
    }
    res
  }

  val example = read(ex)
  val system  = read(xxx)
  val start   = system("AA")
  val paths   = Set[Path]((List(Node(start, -1)), List(Node(start, -1))))

  def reducePaths(in: Set[Path]): Set[Path] = {
    in match {
      case s if s.size < 500 => s
      case s                 =>
        val pressures = s.map(i => (i, valuePath(i))).toList
        val ordered   = pressures.sortBy(_._2)
        val midpoint  = ordered(pressures.length / 3)._2
        pressures.filter(_._2 > midpoint).map(_._1).toSet
    }

  }

  val allPaths = Range
    .inclusive(1, minutes)
    .foldLeft((paths, system)) { case ((p, s), minute) =>
      println(minute)
      val (pathsToActOn, waiting) = p.partition(p => p._1.last.arrived + 2 == minute || p._2.last.arrived + 2 == minute)

      val paths = pathsToActOn.toList match {
        case Nil                   => p
        case somePaths: List[Path] => somePaths.flatMap(p => move(minute, p, s)).toSet
      }
      (reducePaths(paths ++ waiting), s)
    }
    ._1
  println(s"Calculated all possibilities. ${allPaths.size}")

  def valuePath(path: Path): Int = {
    val pressure = 0
    val f1       = Range.inclusive(0, minutes - 1).foldLeft(pressure) { case (pressure, minute) =>
      val me: List[Node]       = path._1.filter(_._2 <= minute - 1).filterNot(_._1.rate == 0)
      val elephant: List[Node] = path._2.filter(_._2 <= minute - 1).filterNot(_._1.rate == 0)
      val on                   = (me ++ elephant).distinctBy(_.valve.name)
      pressure + on.map(_._1.rate).sum
    }
    f1
  }

//  val optEx = (
//    List(
//      Node(example("AA"), -1),
//      Node(example("JJ"), 2),
//      Node(example("BB"), 6),
//      Node(example("CC"), 8),
//    ),
//    List(
//      Node(example("AA"), -1),
//      Node(example("DD"), 1),
//      Node(example("HH"), 6),
//      Node(example("EE"), 10),
//    ),
//  )
//  println(valuePath(optEx))
//  assert(valuePath(optEx) == 1707)

  val pressures = allPaths.map(p => (p, valuePath(p)))
  val best      = pressures.maxBy(_._2)
  println(best._2)
}
