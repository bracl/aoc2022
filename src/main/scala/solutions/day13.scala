package solutions

import utils.IO.*

import scala.language.implicitConversions

object day13 extends App {

  trait Part {
    def compare(p: Part): Int
  }

  case class Empty() extends Part {
    def compare(p: Part): Int = p match {
      case _: Single   => -1
      case _: Sequence => -1
    }
  }

  val empty = Empty()

  case class Single(i: Int) extends Part {
    def compare(p: Part): Int = p match {
      case _: Empty    => 1
      case s: Single   =>
        if (i == s.i) 0
        else if (i < s.i) -1
        else 1
      case s: Sequence => Sequence(List(this)).compare(s)
    }

    override def toString: String = i.toString
  }

  def compareSequences(l0: List[Part], l1: List[Part]): Int = {
    val comp            = l0.zipAll(l1, empty, empty)
    val break           = false
    val start           = 0
    val (comparison, _) = comp.foldLeft((start, break)) { case ((s, break), (x, y)) =>
      if (!break) println(s"comparing: $x => $y")
      if (break) (s, break)
      else {
        val comp: Int = x.compare(y)
        if (comp == 0 && !break)
          (s, break)
        else if (comp == -1 && !break)
          (-1, true)
        else if (comp == 1 && !break)
          (1, true)
        else throw new RuntimeException("ahhhh")
      }
    }
    comparison
  }

  case class Sequence(contents: List[Part]) extends Part {
    def compare(p: Part): Int = p match
      case _: Empty    => 1
      case s: Single   => this.compare(Sequence(List(s)))
      case Sequence(c) => compareSequences(contents, c)

    override def toString: String = s"[${contents.mkString(",")}]"
  }

  case class Packet(contents: List[Part]) {
    override def toString: String = contents.head.toString()
  }

  object Packet {

    def create(s: String): List[Part] = {
      def loop(s: String, acc: List[Part] = Nil): List[Part] = {
        s.toList match {
          case Nil      => List()
          case '[' :: _ =>
            val close: Int        = indexOfClosingBracket(s)
            val part: String      = s.take(close)
            val remaining: String = s.drop(close)
            if (part == s)
              val p = s.drop(1).dropRight(1)
              List(Sequence(loop(p))) ++ loop(remaining)
            else loop(part) ++ loop(remaining)
          case ',' :: _ => acc ++ loop(s.drop(1))
          case _        =>
            val number = s.takeWhile(_.isDigit)
            List(Single(number.mkString("").toInt)) ++ loop(s.drop(number.length))
        }
      }
      loop(s)
    }

    def apply(s: String): Packet = Packet(create(s))
  }

  def indexOfClosingBracket(s: String): Int = {
    assert(s.head == '[')
    val found: Boolean       = false
    val stack: Int           = 0
    val index: Int           = 0
    val charsAcc: List[Char] = List()
    val (_, _, _, ind)       = s.foldLeft((found, charsAcc, stack, index)) { case ((f, ca, s, i), c) =>
      if (!f) {
        c match {
          case '['           => (f, ca.appended(c), s + 1, i + 1)
          case ']' if s == 1 => (true, ca.appended(c), s - 1, i + 1)
          case ']'           => (f, ca.appended(c), s - 1, i + 1)
          case _             => (f, ca.appended(c), s, i + 1)
        }

      } else (f, ca, s, i)

    }
    ind
  }

  val xxx = readGrouped(13)
  println(xxx)

  println("p1")

  val yyy = xxx.map(l => l.map(Packet.apply))

  val index              = 1
  val indexes: List[Int] = List()
  val (p1, ind)          = yyy.foldLeft((index, indexes)) { case ((i, ii), l) =>
    val s    = s"$i " + "=" * 50
    println(s)
    val comp = compareSequences(l.head.contents, l.last.contents)
    println(comp)
    val is   = if (comp < 1) ii.appended(i) else ii
    (i + 1, is)
  }

  println("p2")
  val p2Input    =
    """[[2]]
      |[[6]]""".stripMargin.split("\n").toList
  val allPackets = yyy.flatten ++ p2Input.map(Packet.apply)

  class PacketOrdering extends Ordering[Packet] {
    override def compare(x: Packet, y: Packet): Int = compareSequences(x.contents, y.contents)
  }

  implicit val pO: PacketOrdering = new PacketOrdering()

  println(allPackets)
  val ordered = allPackets.sorted

  val two = ordered.indexOf(Packet.apply(p2Input.head)) + 1
  val six = ordered.indexOf(Packet.apply(p2Input.last)) + 1

  println(ordered.mkString("\n"))

  println(ind.sum)
  println(two * six)

}
