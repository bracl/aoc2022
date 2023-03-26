package solutions

import utils.IO.*

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.Random

object day19 extends App {

  case class State(minute: Int, move: String, state: ResourceMap)
  type SearchStack = Vector[State]

  case class ResourceMap(
    oreRobots: Int = 0,
    ore: Int = 0,
    clayRobots: Int = 0,
    clay: Int = 0,
    obsidianRobots: Int = 0,
    obsidian: Int = 0,
    geodeRobots: Int = 0,
    geode: Int = 0,
  ) {
    def isPositive: Boolean =
      ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0 && oreRobots >= 0 && clayRobots >= 0 && obsidianRobots >= 0 && geodeRobots >= 0

    def totalRobots: Int = oreRobots + clayRobots + obsidianRobots + geodeRobots
  }

  val minute = "minute"

  // robots
  val oreRobots      = "oreRobots"
  val clayRobots     = "clayRobots"
  val obsidianRobots = "obsidianRobots"
  val geodeRobots    = "geodeRobots"
  val robotsSuffix   = "Robots"
  val robots         = Vector(oreRobots, clayRobots, obsidianRobots, geodeRobots)

  // resources
  val ore       = "ore"
  val clay      = "clay"
  val obsidian  = "obsidian"
  val geode     = "geode"
  val resources = List(geode, obsidian, clay, ore)

  case class Blueprint(num: Int, ore: Int, clay: Int, obsidian: (Int, Int), geode: (Int, Int))

  def collectResources(rm: ResourceMap, howManyTimes: Int = 1): ResourceMap = rm.copy(
    ore = rm.ore + rm.oreRobots * howManyTimes,
    clay = rm.clay + rm.clayRobots * howManyTimes,
    obsidian = rm.obsidian + rm.obsidianRobots * howManyTimes,
    geode = rm.geode + rm.geodeRobots * howManyTimes,
  )

  def addRobots(rm: ResourceMap, robot: String): ResourceMap = robot match {
    case g if g == geodeRobots      => rm.copy(geodeRobots = rm.geodeRobots + 1)
    case ob if ob == obsidianRobots => rm.copy(obsidianRobots = rm.obsidianRobots + 1)
    case c if c == clayRobots       => rm.copy(clayRobots = rm.clayRobots + 1)
    case o if o == oreRobots        => rm.copy(oreRobots = rm.oreRobots + 1)
    case s                          => throw new RuntimeException(s"ahhhghghhg => $s")
  }

  def takeResources(rm: ResourceMap, bp: Blueprint, robot: String): ResourceMap = robot match {
    case g if g == geodeRobots =>
      if (debug) println(s"\t\t\tBuilding $robot robot")
      val nOre = rm.ore - bp.geode._1
      val nObs = rm.obsidian - bp.geode._2
      assert(nOre >= 0 && nObs >= 0)
      rm.copy(ore = nOre, obsidian = nObs)

    case ob if ob == obsidianRobots =>
      if (debug) println(s"\t\t\tBuilding $robot robot")
      val nOre  = rm.ore - bp.obsidian._1
      val nClay = rm.clay - bp.obsidian._2
      assert(nOre >= 0 && nClay >= 0)
      rm.copy(ore = nOre, clay = nClay)

    case c if c == clayRobots =>
      if (debug) println(s"\t\t\tBuilding $robot robot")
      val nOre = rm.ore - bp.clay
      assert(nOre >= 0)
      rm.copy(ore = nOre)

    case o if o == oreRobots =>
      if (debug) println(s"\t\t\tBuilding $robot robot")
      val nOre = rm.ore - bp.ore
      assert(nOre >= 0)
      rm.copy(ore = nOre)
    case s                   => throw new RuntimeException(s"jinkies => $s")
  }

  def processMinute(res: ResourceMap, bp: Blueprint, robot: String): ResourceMap = {
    val tr                      = res.totalRobots
    val startBuilding           = takeResources(res, bp, robot)
    assert(startBuilding.isPositive)
    val harvestResources        = collectResources(startBuilding)
    assert(harvestResources.isPositive)
    val ohNiceTheRobotsAreReady = addRobots(harvestResources, robot)
    assert(ohNiceTheRobotsAreReady.isPositive)
    assert(ohNiceTheRobotsAreReady.totalRobots - 1 == tr)
    ohNiceTheRobotsAreReady
  }

  def tooManyRobots(r: ResourceMap, bp: Blueprint): Boolean = {
    r.oreRobots > List(bp.ore, bp.clay, bp.obsidian._1, bp.geode._1).max ||
    r.clayRobots > bp.obsidian._2 ||
    r.obsidianRobots > bp.geode._2
  }

  //  def couldNotReachBestScore(r: ResourceMap, bp: Blueprint, minute: Int, score: Int): Boolean = {
  //    val minutesLeft   = 24 - minute
  //    val currentScore  = geodeCount(r)
  //    val resourcesLeft = ((minutesLeft - 1) * minutesLeft) / 2
  //
  //    val needGeodes        = score + 1 - (currentScore + minutesLeft * geodeRobotCount(r))
  //    val oreForGeodes      = (needGeodes * bp.geode._1) - (minutesLeft * oreRobotCount(r))
  //    val obsidianForGeodes = (needGeodes * bp.geode._2) - (minutesLeft * obsidianRobotCount(r))
  //
  //    val needObsidian    = Math.max(obsidianForGeodes, 0) / bp.geode._2
  //    val oreForObsidian  = (needObsidian * bp.obsidian._1) - (minutesLeft * oreRobotCount(r))
  //    val clayForObsidian = (needObsidian * bp.obsidian._2) - (minutesLeft * clayRobotCount(r))
  //
  //    val needClay   = Math.max(clayForObsidian, 0) / bp.obsidian._2
  //    val oreForClay = (needClay * bp.clay) - (minutesLeft * oreRobotCount(r))
  //
  //    val needOre   = Math.max(oreForClay, 0) / bp.clay
  //    val oreForOre = (needOre * bp.ore) - (minutesLeft * oreRobotCount(r))
  //
  //    val needed = List(oreForOre, oreForClay, oreForObsidian, oreForGeodes, clayForObsidian, obsidianForGeodes)
  //      .map(i => Math.max(i, 0))
  //      .sum
  //
  //    resourcesLeft < needed
  //  }

  def simplerCouldNotReachBestScore(
    r: ResourceMap,
    minute: Int,
    scoreToBeat: Int,
    minuteLimit: Int,
  ): Boolean = {
    val minutesLeft   = minuteLimit + 1 - minute
    val toOvertake    = scoreToBeat + 1 - r.geode
    val resourcesLeft = ((minutesLeft - 1) * minutesLeft) / 2

    toOvertake > resourcesLeft + r.geodeRobots * minutesLeft
  }

  def waitTime(move: String, r: ResourceMap, bp: Blueprint): Option[Int] = move match {
    case o if o == oreRobots  => Some(Math.max(0, Math.ceil((bp.ore - r.ore).toFloat / r.oreRobots.toFloat).toInt))
    case c if c == clayRobots => Some(Math.max(0, Math.ceil((bp.clay - r.ore).toFloat / r.oreRobots.toFloat).toInt))
    case ob if ob == obsidianRobots =>
      if (r.clay > 0)
        if (debug) println("")
        if (debug) println(s"We have clay robots")
        val oreDays  = Math.ceil((bp.obsidian._1 - r.ore).toFloat / r.oreRobots.toFloat).toInt
        val clayDays = Math.ceil((bp.obsidian._2 - r.clay).toFloat / r.clayRobots.toFloat).toInt
        if (debug) println(s"ore:$oreDays clay: $clayDays")
        val res      = List(0, oreDays, clayDays).max
        if (debug) println(res)
        Some(res)
      else
        if (debug) println(s"No clay robots. Can't ever create obsidian robot.")
        None

    case g if g == geodeRobots =>
      if (r.obsidianRobots > 0)
        if (debug) println("")
        if (debug) println(s"We have obsidian robots")
        val oreDays      = Math.ceil((bp.geode._1 - r.ore).toFloat / r.oreRobots.toFloat).toInt
        val obsidianDays = Math.ceil((bp.geode._2 - r.obsidian).toFloat / r.obsidianRobots.toFloat).toInt
        if (debug) println(s"ore:$oreDays obs: $obsidianDays")
        val res          = List(0, oreDays, obsidianDays).max
        if (debug) println(res)
        Some(res)
      else
        if (debug) println(s"No obsidian robots. Can't ever create geode robot.")
        None
  }

  def jumpForward(minute: Int, r: ResourceMap, bp: Blueprint, m: String, minuteLimit: Int): Option[State] = {
    val turnsToWait: Option[Int] = waitTime(m, r, bp)
    if (turnsToWait.isDefined && turnsToWait.get + minute <= minuteLimit)
      turnsToWait.map(t => {
        val resourceMap = collectResources(r, t)
        State(minute + t, m, resourceMap)
      })
    else None
  }

  def canBuildGeode(rm: ResourceMap, bp: Blueprint): Boolean = rm.ore >= bp.geode._1 && rm.obsidian >= bp.geode._2

  def searchBlueprint(bp: Blueprint, minutes: Int = 24): ResourceMap = {
    println(s"Blueprint ${bp.num}")

    val start: ResourceMap       = ResourceMap(oreRobots = 1)
    val unvisited: Vector[State] = Vector(oreRobots, clayRobots).flatMap(m => jumpForward(1, start, bp, m, minutes))

    @tailrec
    def search(best: ResourceMap, unvisited: SearchStack, v: Int = 0): ResourceMap = {
      if (v % 10000 == 0) {
        println(s"$v ${best.geode} ${unvisited.length}")
      }
      if (unvisited.isEmpty) best
      else {
        val State(minute, nextMove, resourceMap) = unvisited.head
        val r: ResourceMap                       = processMinute(resourceMap, bp, nextMove)
        assert(r.isPositive)
        minute match {
          case m if m == minutes =>
            if (r.geode > best.geode) {
              search(r, unvisited.tail, v + 1)
            } else {
              search(best, unvisited.tail, v + 1)
            }

          case i if i < minutes =>
            val movesToAdd: Vector[String] =
              if (tooManyRobots(r, bp)) Vector()
              else if (simplerCouldNotReachBestScore(r, i, best.geode, minutes)) Vector()
              else if (canBuildGeode(r, bp)) Vector(geodeRobots)
              else robots

            val nextLevel =
              movesToAdd.flatMap { move => jumpForward(i + 1, r, bp, move, minutes) }.sortBy(_.minute).reverse

            val prependedUnvisited: SearchStack = nextLevel ++ unvisited.tail
            search(best, prependedUnvisited, v + 1)

          case _ => throw new RuntimeException("whoopsie")
        }
      }
    }

    val bestGeode = search(start, unvisited)
    bestGeode
  }

  val ex  = readExample(19)
  println(ex)
  val xxx = readFile(19)
  println(xxx)

  val in         = xxx
  val blueprints = in.map {
    case s"Blueprint $num: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $ob1 ore and $ob2 clay. Each geode robot costs $ge1 ore and $ge2 obsidian." =>
      Blueprint(num.toInt, ore.toInt, clay.toInt, (ob1.toInt, ob2.toInt), (ge1.toInt, ge2.toInt))
  }.toVector

  val debug = false

  val scores = blueprints.map { bp =>
    val endResource = searchBlueprint(bp, 24)
    val geodes      = endResource.geode
    val quality     = geodes * bp.num
    println(s"${bp.num} * $geodes = ${geodes * bp.num}\n")
    quality
  }
  val p1     = scores.sum
  println(p1)

  val maxGeodes = blueprints.take(3).map { bp =>
    val endResource = searchBlueprint(bp, 32)
    println(endResource.geode)
    endResource.geode
  }

  val p2 = maxGeodes.product
  println(maxGeodes)
  println(p2)

}
