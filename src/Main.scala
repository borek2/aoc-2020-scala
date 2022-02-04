import assignments.Day3

object Main {
  def main(args: Array[String]): Unit = {
    val testSource = scala.io.Source.fromFile("src/resources/Day3Test")
    val testLines = testSource.getLines().toSeq
    Day3.printAnswer1(testLines)
    Day3.printAnswer2(testLines)
    testSource.close()

    val source = scala.io.Source.fromFile("src/resources/Day3")
    val lines = source.getLines().toSeq
    Day3.printAnswer1(lines)
    Day3.printAnswer2(lines)
    testSource.close()
  }
}
