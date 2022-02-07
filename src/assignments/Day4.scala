package assignments

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

  def printAnswer1(input: Seq[String]): Unit = println(countPasswordEntries(input.toList))

  def countPasswordEntries(input: List[String], neededEntries: List[String] = neededEntries): Int = input match {
    case Nil => 0
    case line :: "" :: rest if findPasswordEntries(line.toList, neededEntries).isEmpty => 1 + countPasswordEntries(rest, Day4.neededEntries)
    case line :: Nil => if (findPasswordEntries(line.toList, neededEntries).isEmpty) 1 else 0
    case line :: "" :: rest if findPasswordEntries(line.toList, neededEntries).nonEmpty => countPasswordEntries(rest, Day4.neededEntries)
    case line :: rest => countPasswordEntries(rest, findPasswordEntries(line.toList, neededEntries))
  }

  def findPasswordEntries(line: List[Char], neededEntries: List[String] = neededEntries): List[String] = line match {
    case Nil => neededEntries
    case char1 :: char2 :: char3 :: ':' :: rest =>
      findPasswordEntries(rest, neededEntries.filter(str => str != "".appended(char1).appended(char2).appended(char3)))
    case _ :: rest => findPasswordEntries(rest, neededEntries)
  }

  def entry(line: List[Char]): String = line match {
    case ':' :: _ => ""
    case char :: rest => char + entry(rest)
  }
}
