package solutions

import utils.IO._

object day1 extends App {

  val grouped: List[List[Int]] = readGrouped(1).map(_.map(_.toInt))

  val summs = grouped.map(elf => elf.sum)
  println(summs.max)

  val topThree = summs.sorted.reverse.take(3).sum
  println(topThree)

}
