package solutions

import utils.IO._

object day3 extends App {

  val xxx = readFile(3)
  println(xxx)

  def scoreChar(c: Option[Char]): Int = {
    c match
      case Some(v) if v.isUpper => v.toInt - 38
      case Some(v) if v.isLower => v.toInt - 96
      case _                    => throw new RuntimeException("Arrrggg")
  }

  def sharedItem(s1: Set[Char], s2: Set[Char]) = s1.intersect(s2) match
    case s if s.isEmpty   => None
    case s if s.size == 1 => Some(s.head)
    case _                => throw new RuntimeException("whoops")

  def split(s: String): (Set[Char], Set[Char]) = {
    s.grouped(s.length / 2).map(_.toList).toList match {
      case s1 :: s2 :: Nil => (s1.toSet, s2.toSet)
      case _               => throw new RuntimeException("damn")
    }
  }

  val scores = xxx.map(split).map(sharedItem).map(scoreChar)

  println("p1")
  println(scores.sum)

  def prepGroup(l: List[String]) = (l.head.toList.toSet, l(1).toList.toSet, l(2).toList.toSet)

  def sharedItem3(s1: Set[Char], s2: Set[Char], s3: Set[Char]) = s1.intersect(s2).intersect(s3) match
    case s if s.isEmpty   => None
    case s if s.size == 1 => Some(s.head)
    case _                => throw new RuntimeException("whoops")

  val priorities = xxx.sliding(3, 3).map(prepGroup).map(sharedItem3).map(scoreChar)

  println("p2")
  println(priorities.sum)

}
