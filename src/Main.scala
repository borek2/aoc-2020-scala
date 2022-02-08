import assignments.{Day4}

object Main {
  def main(args: Array[String]): Unit = {
    val testSource = scala.io.Source.fromFile("src/resources/Day4")
    val testLines = testSource.getLines().toSeq
    Day4.printAnswer2(testLines)
    testSource.close()
  }
}
