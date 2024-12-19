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

  "test designs part 1" should {
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

    "part 2" in {
      val (list1, list2) = parse(input)
      underTest.part2(list1, list2) shouldEqual 16
    }
  }

  "tests part 2" should {
    "test" in {
      val patterns = "r, wr, b, g, bwu, rb, gb, br".split(",\\s*").toList

      val trie = underTest.buildTrie(patterns)

      underTest.countDesigns(
        trie,
        "brwrr",
        0,
        scala.collection.mutable.Map.empty
      ) shouldBe 2

      underTest.countDesigns(
        trie,
        "bggr",
        0,
        scala.collection.mutable.Map.empty
      ) shouldBe 1

      underTest.countDesigns(
        trie,
        "rrbgbr",
        0,
        scala.collection.mutable.Map.empty
      ) shouldBe 6

      underTest
        .countDesigns(
          trie,
          "ubwu",
          0,
          scala.collection.mutable.Map.empty
        ) shouldBe 0
    }
  }
}
