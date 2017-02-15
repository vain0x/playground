import scala.io._

object Hello {
  def withFile[X](filePath: String)(f: Source => X): X = {
    val source = Source.fromFile(filePath)
    try {
      f(source)
    } finally {
      source.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val currentDirectory = System.getProperty("user.dir")
    println(s"Current directory: $currentDirectory")

    withFile("input.txt")(source => {
        var i = 1
        for (line <- source.getLines()) {
          println(s"$i: $line")
          i += 1
        }
      }
    )
  }
}
