/**
  * Represents a point in a plane.
  * @param x The X component.
  * @param y The Y component.
  */
case class Point(x: Int, y: Int) {
  def +(r: Point): Point = {
    Point(x + r.x, y + r.y)
  }

  override def toString: String = {
    s"Point($x, $y)"
  }
}

class SuperClass(val x: Int) {
  val y: Int = x + 1
}

class Subclass(x: Int) extends SuperClass(x) {
  val z: Int = x + y
}

object Hello {
  def main(args: Array[String]): Unit = {
    println(s"${Point(1, 2) + Point(10, 10)}")
    println(s"${Point(1, 2).equals(Point(1, 2))}")
    println(s"${new Subclass(2).z}")
  }
}
