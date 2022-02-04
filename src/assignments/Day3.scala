package assignments

import scala.annotation.tailrec

object Day3 {

  class Slope(val right: Int, val down: Int)

  private val answer2Slopes = List(
    new Slope(1, 1),
    new Slope(3, 1),
    new Slope(5, 1),
    new Slope(7, 1),
    new Slope(1, 2)
  )

  private val TREE = '#'

  def printAnswer1(input: Seq[String]): Unit = println(
    countTrees(input.toList, slope = new Slope(3, 1))
  )

  def printAnswer2(input: Seq[String]): Unit = println(
    answer2Slopes.foldLeft(1L)((mult, slope) => mult * countTrees(input.toList, slope = slope))
  )

  private def countTrees(input: List[String], current: Int = 0, stepsTaken: Int = 0, shouldCheckLine: Boolean = false, slope: Slope): Int = shouldCheckLine match {
    case true => getLine(input, current) match {
      case null => 0
      case line if hasTree(line.toList, line.length, stepsTaken) => 1 + countTrees(input, current, stepsTaken, slope = slope)
      case _ => countTrees(input, current, stepsTaken, slope = slope)
    }
    case false => countTrees(input, current + slope.down, stepsTaken + slope.right, shouldCheckLine = true, slope = slope)
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
