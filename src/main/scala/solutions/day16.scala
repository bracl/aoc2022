package solutions

import solutions.day12.{DistanceMap, updateDistanceMap}
import utils.IO.*

import scala.annotation.tailrec

object day16 extends App {

  val ex  = readExample(16)
  println(ex)
  val xxx = readFile(16)
  println(xxx)

  case class Valve(name: String, on: Boolean, rate: Int, paths: List[String])

  type Cave = Map[String, Valve]
  case class Node(valve: Valve, arrived: Int) {
    override def toString: String = s"${valve.name}   $arrived"
  }
  type Path = List[Node]
  type Path2             = List[(Node, Node)]
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
    path: Path,
    minute: Int,
    cave: Cave,
  ): Map[String, (PressurePotential, TimeToGetThere)] = {
    val distanceMap = shortestPath(pos, cave)
    val minutesLeft = 30 - minute
    val moves       = cave.filter { case (k, v) => k != pos && !path.map(_._1).contains(v) }.values
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

  def move(path: Path, cave: Cave): Set[Path] = {

    def addNodeToPath(path: Path, v: Valve, distance: Int): Path = {
      val node = Node(v, path.last.arrived + 1 + distance)
      path :+ node
    }

    val position = path.last
    val pv       = potentialValue(position.valve.name, path, position.arrived, cave)
    val couldMove: List[(String, (PressurePotential, TimeToGetThere))] = pv.toList.sortBy(_._2)
    val shouldMove                                                     = couldMove.filterNot(_._2._1 == 0)
    val res                                                            = shouldMove match {
      case Nil => Set(path)
      case x   => x.map(sM => addNodeToPath(path, cave(sM._1), sM._2._2)).toSet
    }
    res
  }

  val example = read(ex)
  val system  = read(xxx)
  val start   = system("AA")
  val paths   = Set[Path](List(Node(start, -1)))

  val allPaths = Range
    .inclusive(1, 30)
    .foldLeft((paths, system)) { case ((p, s), minute) =>
      println(minute)
      val (pathsToActOn, waiting) = p.partition(p => p.last.arrived + 2 == minute)
      val paths                   = pathsToActOn.toList match {
        case Nil       => p
        case somePaths => somePaths.flatMap(p => move(p, s)).toSet
      }
      (paths ++ waiting, s)
    }
    ._1

  println("p1")

  def valuePath(path: Path): Int = {
    val pressure = 0
    val f1       = Range.inclusive(0, 29).foldLeft(pressure) { case (pressure, minute) =>
      val haveBeenTurnedOn = path.filter(_._2 <= minute - 1).filterNot(_._1.rate == 0)
//      haveBeenTurnedOn match {
//        case Nil =>
//          println(s"== Minute ${minute + 1} == ")
//          println("No valves are open")
//        case _   =>
//          println(s"== Minute ${minute + 1} == ")
//          println(
//            s"Valve(s) ${haveBeenTurnedOn.map(_._1.name).mkString(", ")} are open. releasing ${haveBeenTurnedOn.map(_._1.rate).sum}",
//          )
//      }
      pressure + haveBeenTurnedOn.map(_._1.rate).sum
    }
    f1
  }

  val optEx = List(
    Node(example("AA"), 0),
    Node(example("DD"), 1),
    Node(example("BB"), 4),
    Node(example("JJ"), 8),
    Node(example("HH"), 16),
    Node(example("EE"), 20),
    Node(example("CC"), 23),
  )
  assert(valuePath(optEx) == 1651)

  val pressures = allPaths.map(p => (p, valuePath(p)))
  val best      = pressures.maxBy(_._2)
  println(best._2)
  println("1847 too low")

  println("p2")

}
