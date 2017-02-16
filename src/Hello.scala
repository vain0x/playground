import scala.io._

object Hello {
  def swapArray[X](array: Array[X], i: Int, j: Int) = {
    val t = array(i)
    array(i) = array(j)
    array(j) = t
  }

  def joinByComma(start: Int, end: Int) = {
    (start to end).mkString(",")
  }

  def reverse[X](list: List[X]) = {
    list.foldLeft[List[X]](Nil)((list, x) => x :: list)
  }

  def sum(list: List[Int]) = {
    list.foldRight(0)((x, sum) => x + sum)
  }

  def mkString[T](list: List[T])(sep: String) = {
    list.foldLeft(("", true))((t, x) =>  {
      val (acc, isFirst) = t
      if (isFirst) {
        (x.toString, false)
      } else {
        (acc + sep + x.toString, false)
      }
    })._1
  }

  def main(args: Array[String]): Unit = {
    val array = Array(0, 1, 2, 3, 4)
    swapArray(array, 1, 3)
    println(s"array = ${array.mkString(", ")}")

    println(s"joinByComma(2, 5) = ${joinByComma(2, 5)}")

    println(s"reverse: ${reverse(List(1, 2, 3, 4))}")

    println(s"sum = ${sum(List(2, 3, 5, 7, 11))}")

    println(s"mkString = ${mkString(List(2, 3, 5, 7))(", ")}")
  }
}
