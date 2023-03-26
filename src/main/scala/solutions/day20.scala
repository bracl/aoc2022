package solutions

import utils.IO._

object day20 extends App {

  val ex  = readExample(20).map(_.toLong).toVector
  val xxx = readFile(20).map(_.toLong).toVector

  def insert(vec: Vector[(Long, Int)], i: Int, v: (Long, Int)): Vector[(Long, Int)] = {
    val (s, e) = vec.splitAt(i)
    s ++ Vector(v) ++ e
  }

  def calculateTargetIndex(start: Int, move: Long, length: Int): Int = {
    val actLength = length - 1

    val smallestMove = move - actLength * (move / actLength)

    val t: Int = ((start + smallestMove) % actLength).toInt

    if (t > length) t - actLength
    else if (t < 0) t + actLength
    else t
  }

  def updatedList(pair: (Long, Int), ll: Vector[(Long, Int)], len: Int): Vector[(Long, Int)] = {
//    println("")
//    println(("  " + ll.map(_._1).mkString("  ")).replaceAll(" -", "-"))
    val startIndex     = ll.indexOf(pair)
    val targetIndex    = calculateTargetIndex(startIndex, pair._1, len)
    val withoutElement = ll.patch(startIndex, Nil, 1)
    val updated        = insert(withoutElement, targetIndex, pair)
//    println(pair._1 + " " * (2 - pair._1.toString.length) + " " * (targetIndex * 3) + "V")
//    println(("  " + updated.map(_._1).mkString("  ")).replaceAll(" -", "-"))
    updated
  }

  val in        = xxx
  val withIndex = in.zipWithIndex

  def mix(in: Vector[(Long, Int)]): Vector[(Long, Int)] = {
    val i = in.length
    Range
      .inclusive(0, i - 1)
      .foldLeft(in) { case (vec, index) =>
        val p = vec.find(_._2 == index).get
        updatedList(p, vec, i)
      }

  }
  val res = mix(withIndex).map(_._1)

  def score(vec: Vector[Long]): Long = {
    val startingIndex = vec.indexOf(0)
    val ordinals      = List(1000, 2000, 3000).map { i =>
      vec((startingIndex + i) % vec.length)
    }
    ordinals.sum
  }

  println(score(res))

  val p2withIndex = withIndex.map { case (x, y) => (x * 811589153, y) }

  def mix10(in: Vector[(Long, Int)]): Vector[Long] = {
    val res = Range.inclusive(1, 10).foldLeft(in) { case (vec, i) =>
      println(i)
      val u = mix(vec)
      u
    }
    res.map(_._1)
  }

  println("p2")
  val res2 = mix10(p2withIndex)
  val p2   = score(res2)
  println(p2)
}
