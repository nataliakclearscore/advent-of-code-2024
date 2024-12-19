package com.advent

class Nineteen {

  def possibleDesign(
      patterns: List[String],
      design: String,
      start: Int
  ): Boolean = {
    if (start >= design.length) {
      false
    } else {
      val canContinue =
        patterns.filter(pattern => design.startsWith(pattern, start))
      canContinue.exists(nextPattern =>
        design.substring(start) == nextPattern || possibleDesign(
          patterns,
          design,
          start + nextPattern.length
        )
      )
    }
  }

  def part1(patterns: List[String], designs: List[String]): Int = {
    designs.map(design => possibleDesign(patterns, design, 0)).count(identity)
  }
}
