package com.advent

import org.scalatest.*
import matchers.*
import org.scalatest.wordspec.AnyWordSpec

class OneTest extends AnyWordSpec with should.Matchers {
  "part1" should {
    "work for an example" in {
      val input =
        """3   4
          |4   3
          |2   5
          |1   3
          |3   9
          |3   3""".stripMargin
      new One().part1(input) shouldEqual 11
    }
  }

  "part2" should {
    "work for an example" in {
      val input =
        """3   4
          |4   3
          |2   5
          |1   3
          |3   9
          |3   3""".stripMargin
      new One().part2(input) shouldEqual 31
    }
  }
}
