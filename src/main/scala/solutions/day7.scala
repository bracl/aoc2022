package solutions

import utils.IO._

object day7 extends App {

  val xxx = readFile(7)

  println(xxx)

  case class File(name: String, size: Int)
  case class Dir(name: String, subDirs: Seq[String], files: Seq[File]) {
    def addDir(d: String): Dir = this.copy(subDirs = subDirs.appended(d))
    def addFile(f: File): Dir  = this.copy(files = files.appended(f))

    def size(fs: Map[String, Dir]): Int =
      files.map(_.size).sum + subDirs.map(d => fs(d).size(fs)).sum
  }

  val fileSystem: Map[String, Dir] = Map(
    "/" -> Dir("/", Nil, Nil),
  )

  val workingDirectory: Vector[String] = Vector()

  def cd(s: String, fs: Map[String, Dir], wd: Vector[String]): (Map[String, Dir], Vector[String]) = {
    val dir = s.split(" ").last
    dir match {
      case ".." => (fs, wd.dropRight(1))
      case dir  => (fs, wd.appended(dir))
    }
  }

  def file(s: String, fs: Map[String, Dir], wd: Vector[String]): (Map[String, Dir], Vector[String]) = {
    val f = File(s.split(" ").last, s.split(" ").head.toInt)

    val dirPath         = wd.mkString("/")
    val currentDir: Dir = fs
      .get(dirPath)
      .map(d => d.addFile(f))
      .get
    (fs.updated(dirPath, currentDir), wd)
  }

  def dir(s: String, fs: Map[String, Dir], wd: Vector[String]): (Map[String, Dir], Vector[String]) = {
    val parentPath = wd.mkString("/")
    val name       = s"$parentPath/${s.split(" ").last}"
    val d          = Dir(name, Nil, Nil)

    val parent        = fs(parentPath)
    val updatedParent = parent.addDir(d.name)

    (fs.updated(name, d).updated(parentPath, updatedParent), wd)
  }

  val build = xxx
    .foldLeft((fileSystem, workingDirectory)) { case ((fs, wd), statement) =>
      statement match {
        case s if s.contains(" cd ")  => cd(s, fs, wd)
        case s if s.startsWith("dir") => dir(s, fs, wd)
        case s if s.head.isDigit      => file(s, fs, wd)
        case _                        => (fs, wd)
      }
    }
    ._1

  println("p1")
  println(build)
  private val sizes = build.values.map(d => d.size(build))
  val p1            = sizes.filter(_ <= 100000).sum
  println(p1)

  println("p2")
  val needToDelete = build("/").size(build) - (70000000 - 30000000)
  println(sizes.filter(_ >= needToDelete).min)

}
