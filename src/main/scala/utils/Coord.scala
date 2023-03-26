package utils

import scala.annotation.tailrec
import utils.Coord.given_Conversion_Long_Int

case class Coord(x: Long, y: Long) {
  override def toString: String = s"($x, $y)"

  def +(c: Coord): Coord        = this.add(c)
  def +(t: (Long, Long)): Coord = this.add(Coord(t._1, t._2))
  def add(c: Coord): Coord      = Coord(x + c.x, y + c.y)

  def -(c: Coord): Coord        = this.minus(c)
  def -(t: (Long, Long)): Coord = this.minus(Coord(t._1, t._2))
  def minus(c: Coord): Coord    = Coord(x - c.x, y - c.y)

  def withinBox(minX: Long, maxX: Long, minY: Long, maxY: Long): Boolean = x >= minX &&
    x <= maxX &&
    y >= minY &&
    y <= maxY

  def manhattanDistance(c: Coord): Int = {
    val dx = math.abs(x - c.x)
    val dy = math.abs(y - c.y)
    dx + dy
  }

}

object Coord {
  def apply(x: Long, y: Long): Coord = new Coord(x.toInt, y.toInt)

  given Conversion[Int, Long] with
    def apply(i: Int): Long = i.toLong

  given Conversion[Long, Int] with
    def apply(l: Long): Int =
      if (l < Int.MaxValue && l > Int.MinValue) l.toInt else throw new RuntimeException("Its so fucking big")
}
