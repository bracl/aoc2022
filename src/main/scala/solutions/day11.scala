package solutions

import utils.IO._

object day11 extends App {

  val xxx = readGrouped(11)

  case class Monkey(
    items: List[Long],
    operation: Long => Long,
    divisible: Int,
    test: Long => Int,
    inspected: Int = 0,
  ) {
    override def toString = s"Monkey((${items.mkString(",")}), divisible:$divisible, inspected:$inspected)"

    def throwItem(startingValue: Long, inspectedValue: Long, toMonkey: Monkey): (Monkey, Monkey) = {
      (
        this.copy(items = items.filterNot(_ == startingValue), inspected = inspected + 1),
        toMonkey.copy(items = toMonkey.items.appended(inspectedValue)),
      )
    }
  }
  println(xxx)

  private def sqr(n0: Long): Long                = n0 * n0
  private def multiply(n0: Long)(n1: Long): Long = n0 * n1
  private def add(n0: Long)(n1: Long): Long      = n0 + n1

  private val monkeys = xxx.foldLeft(Map[Int, Monkey]()) { case (monkeys, newM) =>
    val i                 = newM.head.filter(_.isDigit).toInt
    val items: List[Long] =
      newM.find(_.contains("Starting")).get.split(":").last.split(",").map(_.trim.toLong).toList
    val op: Long => Long  = newM.find(_.contains("Operation")).get.split("=").last.trim match {
      case o if o.endsWith("old") => sqr
      case o if o.contains("*")   => multiply(o.filter(_.isDigit).toLong)
      case o if o.contains("+")   => add(o.filter(_.isDigit).toLong)
      case _                      => throw new RuntimeException("Whoops")
    }
    val divisible         = newM.find(_.contains("Test")).get.split("by").last.trim.toInt
    val success           = newM.find(_.contains("true")).get.filter(_.isDigit).toInt
    val failure           = newM.find(_.contains("false")).get.filter(_.isDigit).toInt
    def test(i: Long)     = if (i % divisible == 0) success else failure

    monkeys.updated(i, Monkey(items, op, divisible, test))
  }

  private def reduceWorry(bi: Long, nextDiv: Long): Long = bi % nextDiv

  def run(ms: Map[Int, Monkey], round: Boolean = true): Map[Int, Monkey] = {
    val commonMultiple = ms.values.map(_.divisible).product
    val a              = ms.keys.toList.sorted.foldLeft(ms) { case (monkeys, index) =>
      val monkey  = monkeys(index)
      val updated = monkey.items
        .foldLeft((monkeys, monkey)) { case ((monkeys, monkey), item) =>
          val opped = monkey.operation(item)

          val (newWorry, target) = if (round) {
            val worry       = math.floor(opped.toFloat / 3).toLong
            val destination = monkey.test(worry)
            (worry, destination)
          } else {

            val destination = monkey.test(opped)
            val worry: Long = reduceWorry(opped, commonMultiple)
            (worry, destination)

          }

          val (source, targetMonkey) = monkey.throwItem(item, newWorry, monkeys(target))
          (monkeys.updated(index, source).updated(target, targetMonkey), source)
        }
        ._1
      updated
    }
    a
  }

  private val twenty = Range.inclusive(1, 20).foldLeft(monkeys) { case (m, _) =>
    run(m)
  }
  println(twenty.values.toList.sortBy(_.inspected).reverse.take(2).map(_.inspected).product)

  private val tenThousand = Range.inclusive(1, 10000).foldLeft(monkeys) { case (m, _) =>
    run(m, round = false)
  }
  println(tenThousand.values.toList.sortBy(_.inspected).reverse.take(2).map(_.inspected.toLong).product)

}
