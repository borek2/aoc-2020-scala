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

  def printAnswer2(input: Seq[String]): Unit = println(countValidPassports2(input.toList))

  def countValidPassports(input: List[String], neededEntries: List[String] = neededEntries): Int = input match {
    case Nil => neededEntries.isEmpty.toInt
    case "" :: rest => neededEntries.isEmpty.toInt + countValidPassports(rest, Day4.neededEntries)
    case line :: rest => countValidPassports(rest, filterNeededEntries(line.toList, neededEntries))
  }

  def countValidPassports2(input: List[String], neededEntries: List[String] = neededEntries): Int = input match {
    case Nil => neededEntries.isEmpty.toInt
    case "" :: rest => neededEntries.isEmpty.toInt + countValidPassports2(rest, Day4.neededEntries)
    case line :: rest => countValidPassports2(rest, filterNeededEntries2(line.toList, neededEntries))
  }

  @tailrec
  def filterNeededEntries(line: List[Char], neededEntries: List[String] = neededEntries): List[String] = line match {
    case Nil => neededEntries
    case char1 :: char2 :: char3 :: ':' :: rest =>
      filterNeededEntries(rest, neededEntries.filter(str => str != (char1 :: char2 :: char3 :: Nil).stringify))
    case _ :: rest => filterNeededEntries(rest, neededEntries)
  }

  def filterNeededEntries2(line: List[Char], neededEntries: List[String] = neededEntries): List[String] = line match {
    case Nil => neededEntries
    case char1 :: char2 :: char3 :: ':' :: rest if line.isValid =>
      filterNeededEntries2(rest, neededEntries.filter(str => str != (char1 :: char2 :: char3 :: Nil).stringify))
    case _ :: rest => filterNeededEntries2(rest, neededEntries)
  }

  /**
   * byr (Birth Year) - four digits; at least 1920 and at most 2002.
   * iyr (Issue Year) - four digits; at least 2010 and at most 2020.
   * eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
   * hgt (Height) - a number followed by either cm or in:
   * If cm, the number must be at least 150 and at most 193.
   * If in, the number must be at least 59 and at most 76.
   * hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
   * ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
   * pid (Passport ID) - a nine-digit number, including leading zeroes.
   * cid (Country ID) - ignored, missing or not.
   */

  implicit class asPasswordEntryValue(chars: List[Char]) {
    def isValid: Boolean = chars match {
      case 'b' :: 'y' :: 'r' :: ':' :: rest => are4DigitsBetween(1920, 2002, rest)
      case 'i' :: 'y' :: 'r' :: ':' :: rest => are4DigitsBetween(2010, 2020, rest)
      case 'e' :: 'y' :: 'r' :: ':' :: rest => are4DigitsBetween(2020, 2030, rest)
      case 'h' :: 'g' :: 't' :: ':' :: rest => isValidHgt(rest)
      case 'h' :: 'c' :: 'l' :: ':' :: rest => isValidHcl(rest)
      case 'e' :: 'c' :: 'l' :: ':' :: rest => isValidEcl(rest)
      case 'p' :: 'i' :: 'd' :: ':' :: rest => isValidPid(rest)
      case _ => true
    }

    private def are4DigitsBetween(low: Int, high: Int, chars: List[Char]): Boolean = chars match {
      case char1 :: char2 :: char3 :: char4 :: tail if tail == Nil || tail.head == ' ' =>
        chars.take(4).forall(_.isDigit) && s"${char1}${char2}${char3}${char4}".toInt >= low && s"${char1}${char2}${char3}${char4}".toInt <= high
      case _ => false
    }

    private def isValidHgt(chars: List[Char], currentNumberString: String = ""): Boolean = chars match {
      case 'i' :: 'n' :: tail if tail == Nil || tail.head == ' ' && currentNumberString.nonEmpty => currentNumberString.toInt >= 59 && currentNumberString.toInt <= 76
      case 'c' :: 'm' :: tail if tail == Nil || tail.head == ' ' && currentNumberString.nonEmpty => currentNumberString.toInt >= 150 && currentNumberString.toInt <= 193
      case char :: rest if char.isDigit => isValidHgt(rest, s"$currentNumberString$char")
      case _ => false
    }

    private def isValidHcl(chars: List[Char]): Boolean = chars match {
      case '#' :: char1 :: char2 :: char3 :: char4 :: char5 :: char6 :: tail if tail == Nil || tail.head == ' ' =>
        s"$char1$char2$char3$char4$char5$char6".matches("[0-9a-z]+")
      case _ => false
    }

    private def isValidEcl(chars: List[Char]): Boolean = chars match {
      case char1 :: char2 :: char3 :: tail if tail == Nil || tail.head == ' ' => s"$char1$char2$char3".matches("amb|blu|brn|gry|grn|hzl|oth")
      case _ => false
    }

    private def isValidPid(chars: List[Char]): Boolean = chars match {
      case _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: tail if tail == Nil || tail.head == ' ' => chars.take(9).forall(_.isDigit)
      case _ => false
    }
  }

  implicit class asInt(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  implicit class asString(chars: List[Char]) {
    def stringify: String = chars.foldLeft("") { (a, b) => a.appended(b) }
  }
}
