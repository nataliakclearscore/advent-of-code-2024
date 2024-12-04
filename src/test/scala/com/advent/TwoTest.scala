package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwoTest extends AnyWordSpec with should.Matchers {
  "part1Lists" should {
    "work for an example" in {
      val input =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9""".stripMargin
      new Two().part1Lists(input) shouldEqual List(true, false, false, false,
        false, true)
    }
  }

  "part1" should {
    "work for an example" in {
      val input =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9""".stripMargin
      new Two().part1(input) shouldEqual 2
    }
  }

  "part2Lists" should {
    "work for an example" in {
      val input =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9""".stripMargin
      new Two().part2Lists(input) shouldEqual List(true, false, false, true,
        true, true)
    }

    "work for line" in {
      val input: String = "48 46 47 49 51 54 56"
      new Two().part2Lists(input) shouldEqual List(true)
    }

    "work on edge cases" in {
      val input =
        """48 46 47 49 51 54 56
          |1 1 2 3 4 5
          |1 2 3 4 5 5
          |5 1 2 3 4 5
          |1 4 3 2 1
          |1 6 7 8 9
          |1 2 3 4 3
          |9 8 7 6 7
          |7 10 8 10 11
          |29 28 27 25 26 25 22 20""".stripMargin
      new Two().part2Lists(input) shouldEqual List(true, true, true, true, true,
        true, true, true, true, true)
    }
  }

  "part2" should {
    "work for an example" in {
      val input =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9""".stripMargin
      new Two().part2(input) shouldEqual 4
    }
  }
}
