package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class NineteenTest extends AnyWordSpec with should.Matchers {

  def parse(input: String): (List[String], List[String]) = {
    val lines = input.split("\n").map(_.trim)
    val list1: List[String] = lines.head.split(",\\s*").toList
    val list2: List[String] = lines.tail.filterNot(_ == "").toList
    (list1, list2)
  }

  private val underTest = new Nineteen

  "test designs" should {
    "test" in {
      val patterns = "r, wr, b, g, bwu, rb, gb, br".split(",\\s*").toList
      underTest.possibleDesign(patterns, "brwrr", 0) shouldEqual true
      underTest.possibleDesign(patterns, "ubwu", 0) shouldEqual false
    }
  }

  "examle 1" should {
    val input = """r, wr, b, g, bwu, rb, gb, br
                  |
                  |brwrr
                  |bggr
                  |gbbr
                  |rrbgbr
                  |ubwu
                  |bwurrg
                  |brgr
                  |bbrgwb""".stripMargin
    "part 1" in {
      val (list1, list2) = parse(input)
      underTest.part1(list1, list2) shouldEqual 6
    }
  }
}
