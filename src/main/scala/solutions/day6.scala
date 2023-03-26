package solutions

import utils.IO._

object day6 extends App {

  val xxx = readFile(6).head
  println(xxx)

  def packets(firstn: Int, input: String = xxx): Int =
    input.sliding(firstn).takeWhile(l => l.toSet.size != firstn).length + firstn

  println("p1")
  println(packets(4))

  println("p2")
  println(packets(14))

}
