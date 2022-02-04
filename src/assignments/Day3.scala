package assignments

import scala.annotation.tailrec

object Day3_1 {

  private val RIGHT_COUNT = 3
  private val TREE = '#'

  def printAnswer(input: Seq[String]): Unit = println(countTrees(input.toList))

  private def countTrees(input: List[String], index: Int = 0, stepsRight: Int = 0, check: Boolean = false): Int = check match {
    case true => getLine(input, index) match {
      case null => 0
      case line if hasTree(line.toList, line.length, stepsRight) => 1 + countTrees(input, index, stepsRight)
      case _ => countTrees(input, index, stepsRight)
    }
    case false => countTrees(input, index + 1, stepsRight + RIGHT_COUNT, check = true)
  }

  @tailrec
  private def getLine(input: List[String], index: Int, count: Int = 0): String = input match {
    case Nil => null
    case head :: _ if count == index => head
    case _ :: tail => getLine(tail, index, count + 1)
  }

  @tailrec
  private def hasTree(line: List[Char], lineLength: Int, index: Int, current: Int = 0): Boolean = line match {
    case Nil => false
    case char :: _ if current == (index % lineLength) => char == TREE
    case _ :: rest => hasTree(rest, lineLength, index, current + 1)
  }
}
