package assignments

import scala.annotation.tailrec

/**
 * byr (Birth Year)
 * iyr (Issue Year)
 * eyr (Expiration Year)
 * hgt (Height)
 * hcl (Hair Color)
 * ecl (Eye Color)
 * pid (Passport ID)
 * cid (Country ID)
 */
object Day4 {

  val neededEntries = List(
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"
  )

  def printAnswer1(input: Seq[String]): Unit = println(countValidPassports(input.toList))

  def countValidPassports(input: List[String], neededEntries: List[String] = neededEntries): Int = input match {
    case Nil => neededEntries.isEmpty.toInt
    case "" :: rest => neededEntries.isEmpty.toInt + countValidPassports(rest, Day4.neededEntries)
    case line :: rest => countValidPassports(rest, filterNeededEntries(line.toList, neededEntries))
  }

  @tailrec
  def filterNeededEntries(line: List[Char], neededEntries: List[String] = neededEntries): List[String] = line match {
    case Nil => neededEntries
    case char1 :: char2 :: char3 :: ':' :: rest =>
      filterNeededEntries(rest, neededEntries.filter(str => str != (char1 :: char2 :: char3 :: Nil).stringify))
    case _ :: rest => filterNeededEntries(rest, neededEntries)
  }

  implicit class asInt(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  implicit class asString(chars: List[Char]) {
    def stringify: String = chars.foldLeft("") { (a, b) => a.appended(b) }
  }
}
