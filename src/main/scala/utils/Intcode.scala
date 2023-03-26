package utils

import scala.annotation.tailrec

case class Intcode(program: Vector[Int], input: Int, debug: Boolean = false) {

  def nounAndVerb(noun: Int, verb: Int): Intcode =
    this.copy(program = program.updated(1, noun).updated(2, verb))

  def interpretOpcode(i: Int): (Int, Seq[Int]) = {
    val reversed = i.toString.toList.reverse.padTo(5, '0')
    val opcode   = reversed.take(2).reverse.mkString("").toInt
    val params   = reversed.drop(2).map(_.toString.toInt)
    (opcode, params)
  }

  def valueBasedOnMode(program: Vector[Int], position: Int, mode: Int): Int = {
    val res = mode match {
      case 0 => program(program(position))
      case 1 => program(position)
      case _ => throw RuntimeException(s"Mode: $position, $mode")
    }
    if (debug) println(s"res: $res")
    res
  }

  def addressBasedOnMode(program: Vector[Int], position: Int, mode: Int): Int = {
    val add = mode match {
      case 0 => program(position)
      case 1 => position
      case _ => throw RuntimeException(s"Address: $position, $mode")
    }
    if (debug) println(s"add: $add")
    add
  }

  def print(program: Vector[Int], position: Int, n: Int = 3): Unit = {
    val (op, params) = interpretOpcode(program(position))
    val nextN        = (1 to n).map(i => program(position + i))
    if (debug) println(s"[op:$op, params:$params, next: ${nextN.mkString(", ")}")
  }

  @tailrec
  final def compute(
    program: Vector[Int] = this.program,
    position: Int = 0,
    outputs: Seq[Int] = Nil,
  ): (Int, Seq[Int], Vector[Int]) = {
    interpretOpcode(program(position)) match {
      case (99, _)                    => (program.head, outputs, program)
      case (1, p1 :: p2 :: p3 :: Nil) =>
        print(program, position)
        compute(
          program.updated(
            addressBasedOnMode(program, position + 3, p3),
            valueBasedOnMode(program, position + 1, p1) + valueBasedOnMode(program, position + 2, p2),
          ),
          position + 4,
          outputs,
        )
      case (2, p1 :: p2 :: p3 :: Nil) =>
        print(program, position)
        compute(
          program.updated(
            addressBasedOnMode(program, position + 3, p3),
            valueBasedOnMode(program, position + 1, p1) * valueBasedOnMode(program, position + 2, p2),
          ),
          position + 4,
          outputs,
        )
      case (3, _ :: _ :: _ :: Nil)    =>
        print(program, position, 1)
        compute(program.updated(addressBasedOnMode(program, position + 1, 0), input), position + 2, outputs)

      case (4, p1 :: _ :: _ :: Nil) =>
        print(program, position, 1)
        compute(program, position + 2, outputs :+ valueBasedOnMode(program, position + 1, p1))

      case (5, p1 :: p2 :: _ :: Nil) =>
        if (valueBasedOnMode(program, position + 1, p1) != 0)
          compute(program, valueBasedOnMode(program, position + 2, p2), outputs)
        else compute(program, position + 3, outputs)

      case (6, p1 :: p2 :: _ :: Nil) =>
        if (valueBasedOnMode(program, position + 1, p1) == 0)
          compute(program, valueBasedOnMode(program, position + 2, p2), outputs)
        else compute(program, position + 3, outputs)

      case (7, p1 :: p2 :: p3 :: Nil) =>
        val valueToWrite =
          if (valueBasedOnMode(program, position + 1, p1) < valueBasedOnMode(program, position + 2, p2)) 1 else 0
        compute(program.updated(addressBasedOnMode(program, position + 3, p3), valueToWrite), position + 4, outputs)

      case (8, p1 :: p2 :: p3 :: Nil) =>
        val valueToWrite =
          if (valueBasedOnMode(program, position + 1, p1) == valueBasedOnMode(program, position + 2, p2)) 1 else 0
        compute(program.updated(addressBasedOnMode(program, position + 3, p3), valueToWrite), position + 4, outputs)

      case _ => throw RuntimeException("whoops")
    }
  }
}

object Intcode {
  private def toInput(s: String): Vector[Int]       = s.split(",").toVector.map(_.toInt)
  def apply(s: String): Intcode                     = Intcode(toInput(s), 0)
  def apply(vi: Vector[Int]): Intcode               = Intcode(vi, 0)
  def apply(s: String, i: Int): Intcode             = Intcode(toInput(s), i)
  def apply(s: String, i: Int, b: Boolean): Intcode = Intcode(toInput(s), i, b)
}

