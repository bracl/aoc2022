package solutions

import utils.IO._

object day25 extends App {

  val ex  = readExample(25)
  println(ex)
  val xxx = readFile(25)
  println(xxx)

  def dealWithMinus(c: Char): Int = {
    c match {
      case '-'                                    => -1
      case '='                                    => -2
      case x if x.isDigit && x.toString.toInt < 5 => x.toString.toInt
      case _                                      => throw new RuntimeException("dsklfjsdlkfjs")
    }
  }

  def toDecimalPlaces(s: String): List[Int] = {
    s.toList.map(dealWithMinus)
  }

  def snafuToDecimal(s: String): Long = {
    val asNumbers = toDecimalPlaces(s)
    val asFives   = asNumbers.reverse.zipWithIndex.map { case (num, index) =>
      (num * Math.pow(5, index)).toLong
    }
    asFives.sum
  }

  println("p1")
  snafuToDecimal("2=")
  println(ex.map(snafuToDecimal))
  println(List(1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3, 37))
  val decimal = xxx.map(snafuToDecimal).sum

//  def closestFive(i: Int): Int = {
//    val closest = 99999
//    Range.inclusive(1, 100).foldLeft((i, closest)) { case ((searchingFor, x), i) =>
//      val indexesToConsider = Range.inclusive(i, i - 2, -1).takeWhile(_ >= 0)
//
//
//      (searchingFor, x)
//    }
//  }

  def goAlmostUp(s: String, i: Long): String = {
    if (snafuToDecimal(s) < i && snafuToDecimal(s.replaceAll("=", "2")) > i)
      assert(snafuToDecimal(s) < i)
      s
    else goAlmostUp(s"$s=", i)
  }

  def goUp(s: String, i: Long): String = {
    val len = s.length

    Range.inclusive(0, len - 1).foldLeft(s) { case (snafu, index) =>
      if (index == 0) snafu
      else {
        println(s"$i ${snafuToDecimal(snafu)} ${math.abs(i - snafuToDecimal(snafu))}")
        val chars          = List('=', '-', '0', '1', '2')
        val candidates     = chars.map(c => snafu.updated(index, c))
        val tested         = candidates.takeWhile(c => snafuToDecimal(c) <= i)
        assert(tested.nonEmpty)
        val highestWeCanGo = tested.last
        highestWeCanGo
      }
    }
  }

  val x = "2=1-=0=============="
  val s = snafuToDecimal(x)
  println(decimal)
  println(s)
  if (s > decimal) println(s"smaller -> ${s - decimal}")
  else println(s"bigger -> ${decimal - s}")

  val startingSnafu = goAlmostUp("2", decimal)
  println(startingSnafu)

  val snafu = goUp(startingSnafu, decimal)
  println(snafu)

  println(decimal)
  println(snafuToDecimal(snafu))

}
