package com.advent

class Three {

  // returns the number as string starting from the index, empty if no number
  def parseNum(input: String, start: Int): String = {
    var res = ""
    var i = start
    while (i < input.length && input(i).isDigit) {
      res += input(i)
      i += 1
    }
    res
  }

  private val Prefix: String = "mul("
  private val Comma: Char = ','
  private val Suffix: Char = ')'

  // pattern: mul(<int>,<int>)
  // returns either the index where it failed to match or two numbers to multiply (as Strings)
  def parseMul(input: String, start: Int): Option[(String, String)] = {
    if (
      start + Prefix.length < input.length && input.substring(
        start,
        start + Prefix.length
      ) == Prefix
    ) {
      val num1 = parseNum(input, start + Prefix.length)
      val commaI = start + Prefix.length + num1.length
      if (num1.nonEmpty && commaI < input.length && input(commaI) == Comma) {
        val num2 = parseNum(input, start + Prefix.length + num1.length + 1)
        val suffixI = start + Prefix.length + num1.length + 1 + num2.length
        if (
          num2.nonEmpty && suffixI < input.length && input(suffixI) == Suffix
        ) {
          Some((num1, num2))
        } else {
          None
        }
      } else {
        None
      }
    } else {
      None
    }
  }

  def parseAllPart1(input: String): List[(String, String)] = {
    var i = 0
    var res = List[(String, String)]()
    while (i < input.length) {
      parseMul(input, i) match {
        case Some((num1, num2)) =>
          res = (num1, num2) :: res
          i += num1.length + num2.length + 6
        case None =>
          i += 1
      }
    }
    res
  }
  
  def part1(input: String): Long = {
    val parsed = parseAllPart1(input)
    parsed.map { case (num1, num2) => num1.toLong * num2.toLong }.sum
  }
}
