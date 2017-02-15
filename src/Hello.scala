import scala.util._
import java.security._

object Hello {
  def main(args: Array[String]): Unit = {
    val random = new Random(new SecureRandom())
    for (_ <- 1 to 1000) {
      val s = random.alphanumeric.take(4).mkString
      val t = s + s.charAt(0)
      println(t)
    }
  }
}
