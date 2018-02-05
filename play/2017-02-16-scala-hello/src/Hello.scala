import scala.io._

sealed abstract class DayOfWeek {
  def next: DayOfWeek = {
    this match {
      case Sunday => Monday
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
    }
  }
}

case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek

object Hello {
  def main(args: Array[String]): Unit = {
    for (dayOfWeek <- List(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)) {
      println(s"$dayOfWeek, next = ${dayOfWeek.next}")
    }
  }
}
