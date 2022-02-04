package assignments

import scala.annotation.tailrec

object Day3_1 {

  private val AMOUNT_OF_STEPS_RIGHT = 3
  private val TREE = '#'

  def printAnswer(input: Seq[String]): Unit = println(countTrees(input.toList))

  private def countTrees(input: List[String], current: Int = 0, stepsTaken: Int = 0, shouldCheckLine: Boolean = false): Int = shouldCheckLine match {
    case true => getLine(input, current) match {
      case null => 0
      case line if hasTree(line.toList, line.length, stepsTaken) => 1 + countTrees(input, current, stepsTaken)
      case _ => countTrees(input, current, stepsTaken)
    }
    case false => countTrees(input, current + 1, stepsTaken + AMOUNT_OF_STEPS_RIGHT, shouldCheckLine = true)
  }

  @tailrec
  private def getLine(input: List[String], index: Int, current: Int = 0): String = input match {
    case Nil => null
    case head :: _ if current == index => head
    case _ :: tail => getLine(tail, index, current + 1)
  }

  @tailrec
  private def hasTree(line: List[Char], lineLength: Int, index: Int, current: Int = 0): Boolean = line match {
    case Nil => false
    case char :: _ if current == (index % lineLength) => char == TREE
    case _ :: rest => hasTree(rest, lineLength, index, current + 1)
  }
}
