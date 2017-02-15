import scala.util._
import java.security._

/**
  * Represents a point in a plane.
  * @param x
  * @param y
  */
class Point(val x: Int, val y: Int) {
  def +(r: Point) = {
    new Point(x + r.x, y + r.y)
  }

  override def toString: String = {
    s"Point(${x}, ${y})"
  }
}

object Hello {
  def main(args: Array[String]): Unit = {
    println(s"${new Point(1, 2) + new Point(10, 10)}")
  }
}
