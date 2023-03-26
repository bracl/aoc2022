package solutions

import utils.IO.*

import scala.annotation.tailrec
import scala.util.Try

object day21 extends App {

  val ex  = readExample(21)
  println(ex)
  val xxx = readFile(21)
  println(xxx)

  def perform(m0: Long, m1: Long, op: String): Long = {
    op match {
      case s if s.contains("*") => m0 * m1
      case s if s.contains("+") => m0 + m1
      case s if s.contains("-") => m0 - m1
      case s if s.contains("/") => m0 / m1
      case _                    => throw new RuntimeException("arse")
    }
  }

  case class HUMN() extends Throwable {
    override def toString: String = "ME???"
  }

  private val humn = "humn"

  def getMonkey(monkey: String, mm: Map[String, String], p2: Boolean = false): Long = {
    if (p2 && monkey == humn)
      throw new HUMN

    val maybeMonkey: String = mm(monkey)
    if (maybeMonkey.forall(_.isDigit)) maybeMonkey.toLong
    else {
      val m0 = getMonkey(maybeMonkey.split(" ").head, mm, p2)
      val m1 = getMonkey(maybeMonkey.split(" ").last, mm, p2)
      perform(m0, m1, maybeMonkey)
    }
  }

  println("p1")
  val in                             = xxx
  val monkeyMap: Map[String, String] = in.flatMap { l =>
    val spl = l.split(":").toList
    spl match {
      case a :: b :: Nil => Seq(a.trim -> b.trim)
    }
  }.toMap
  println(getMonkey("root", monkeyMap))

  @tailrec
  def pathToRoot(start: String, path: List[String], mm: Map[String, String]): List[String] = {
    mm.toList.find(_._2.contains(start)) match {
      case Some((k, _)) if k.contains("root") => path.appended("root")
      case Some((k, _))                       =>
        val p = path.appended(k)
        pathToRoot(k, p, mm)
      case None                               => throw new RuntimeException("boooo")
    }
  }

  println("p2")
  val pathWithHumn = pathToRoot(humn, List(humn), monkeyMap)
  println(pathWithHumn)
  val hqpw         = "hqpw" // contains humn
  val nprj         = "nprj"
  val valueToMatch = getMonkey(nprj, monkeyMap)

  def unpickMonkey(monkey: String, expectedValue: Long, mm: Map[String, String]): Long = {

    if (monkey == humn)
      return expectedValue

    def unpick(m0: (String, Option[Long]), m1: (String, Option[Long]), op: String, ex: Long): Long = {
      (m0, m1, op) match {
        // ex = n0 + n1
        case ((n0, None), (n1, Some(m)), s) if s.contains("+") =>
          val exp = ex - m
          unpickMonkey(n0, exp, mm)
        case ((n0, Some(m)), (n1, None), s) if s.contains("+") =>
          val exp = ex - m
          unpickMonkey(n1, exp, mm)

        // ex = n0 - n1
        case ((n0, None), (n1, Some(m)), s) if s.contains("-") =>
          val exp = ex + m
          unpickMonkey(n0, exp, mm)
        case ((n0, Some(m)), (n1, None), s) if s.contains("-") =>
          val exp = m - ex
          unpickMonkey(n1, exp, mm)

        // ex = n0 / n1
        case ((n0, None), (n1, Some(m)), s) if s.contains("/") =>
          val exp = ex * m
          unpickMonkey(n0, exp, mm)
        case ((n0, Some(m)), (n1, None), s) if s.contains("/") =>
          val exp = m * ex
          unpickMonkey(n1, exp, mm)

        // ex = n0 * n1
        case ((n0, None), (n1, Some(m)), s) if s.contains("*") =>
          val exp = ex / m
          unpickMonkey(n0, exp, mm)
        case ((n0, Some(m)), (n1, None), s) if s.contains("*") =>
          val exp = ex / m
          unpickMonkey(n1, exp, mm)
        case _                                                 => throw new RuntimeException("wank")
      }
    }

    val maybeMonkey = mm(monkey)
    val m0          =
      val n0 = maybeMonkey.split(" ").head
      try {
        (n0, Some(getMonkey(n0, mm, true)))
      } catch {
        case _: HUMN => (n0, None)
      }
    val m1          =
      val n1 = maybeMonkey.split(" ").last
      try {
        (n1, Some(getMonkey(n1, mm, true)))
      } catch {
        case _: HUMN => (n1, None)
      }

    val a = unpick(m0, m1, maybeMonkey, expectedValue)
    a
  }

  println(unpickMonkey(hqpw, valueToMatch, monkeyMap))

}
