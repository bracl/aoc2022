package solutions

import utils.IO._

object day4 extends App {

  val xxx = readFile(4)
  val yyy = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8",
  )
  println(xxx)

  def isSubset(s1: Set[Int], s2: Set[Int]) = s1.subsetOf(s2) || s2.subsetOf(s1)
  def overlap(s1: Set[Int], s2: Set[Int])  = s1.intersect(s2).nonEmpty

  def toSets(s: String): (Set[Int], Set[Int]) = {
    def toSet(s: String): Set[Int] = {
      s.split("-").toList match
        case x :: y :: Nil if x == y => Set(x.toInt)
        case x :: y :: Nil           => Range.inclusive(x.toInt, y.toInt).toSet
        case _                       => throw new RuntimeException("heavens")
    }

    s.split(",").toList match
      case a :: b :: Nil => (toSet(a), toSet(b))
      case _             => throw new RuntimeException("Darn")
  }

  private val sets = xxx.map(toSets)
  val p1           = sets.count(isSubset)
  println("p1")
  println(p1)

  val p2 = sets.count(overlap)
  println("p2")
  println(p2)

}
