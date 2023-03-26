package solutions

import utils.IO._
import utils.Utils.TakeUntilListWrapper

object day8 extends App {

  val xxx = readFile(8).map(_.map(_.toInt).toVector).toVector
//  val xxx = Vector(Vector(3, 0, 3, 7, 3), Vector(2, 5, 5, 1, 2), Vector(6, 5, 3, 3, 2), Vector(3, 3, 5, 4, 9), Vector(3, 5, 3, 9, 0))
  println(xxx)

  def visible(x: Int, y: Int, grid: Vector[Vector[Int]]): Boolean = {
    if (x == 0 || x == grid.head.length || y == 0 || y == grid.length) true
    else {
      val row       = grid(y)
      val column    = grid.map(_(x))
      val treeValue = row(x)
      assert(treeValue == column(y))
      val left      = row.take(x).maxOption.forall(_ < treeValue)
      val right     = row.takeRight((row.length - x) - 1).maxOption.forall(_ < treeValue)
      val top       = column.take(y).maxOption.forall(_ < treeValue)
      val bottom    = column.takeRight((column.length - y) - 1).maxOption.forall(_ < treeValue)
      left || right || top || bottom
    }
  }

  def scenicScore(x: Int, y: Int, grid: Vector[Vector[Int]]): Int = {
    val row       = grid(y)
    val column    = grid.map(_(x))
    val treeValue = row(x)
    assert(treeValue == column(y))

    val left: Int  = row.take(x).reverse.toList.takeUntil(_ < treeValue).length
    val right: Int = row.takeRight((row.length - x) - 1).toList.takeUntil(_ < treeValue).length
    val up: Int    = column.take(y).reverse.toList.takeUntil(_ < treeValue).length
    val down: Int  = column.takeRight((column.length - y) - 1).toList.takeUntil(_ < treeValue).length
    left * right * up * down
  }

  val trees = for {
    x <- Range(0, xxx.head.length)
    y <- Range(0, xxx.length)
  } yield (x, y, visible(x, y, xxx), scenicScore(x, y, xxx))

  println("p1")
  trees.foreach(println)
  println(trees.count(_._3 == true))

  println("p2")
  println(trees.maxBy(_._4)._4)

}
