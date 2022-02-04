import assignments.Day3_1

object Main {
  def main(args: Array[String]): Unit = {
    val testSource = scala.io.Source.fromFile("src/resources/Day3Test")
    Day3_1.printAnswer(testSource.getLines().toSeq)
    testSource.close()
//
//    val source = scala.io.Source.fromFile("src/resources/Day3")
//    Day3.printAnswers(testSource.getLines().toSeq)
//    testSource.close()
  }
}
