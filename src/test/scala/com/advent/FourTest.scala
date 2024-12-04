package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class FourTest extends AnyWordSpec with should.Matchers {
  "findAll" should {
    "work for indexes" in {
      val input =
        """MMMSXXMASM
          |MSAMXMSMSA
          |AMXSXMAAMM
          |MSAMASMSMX
          |XMASAMXAMM
          |XXAMMXXAMA
          |SMSMSASXSS
          |SAXAMASAAA
          |MAMMMXMMMM
          |MXMXAXMASX""".stripMargin
      val arr: Array[Array[Char]] = input.split("\n").map(_.trim.toCharArray)
      new Four().findAll(arr, 0, 4) shouldEqual 1
      new Four().findAll(arr, 0, 5) shouldEqual 1
      new Four().findAll(arr, 1, 4) shouldEqual 1
      new Four().findAll(arr, 3, 9) shouldEqual 2
      new Four().findAll(arr, 4, 0) shouldEqual 1
      new Four().findAll(arr, 4, 6) shouldEqual 2
      new Four().findAll(arr, 5, 0) shouldEqual 1
      new Four().findAll(arr, 5, 6) shouldEqual 1
    }
  }

  "part1" should {
    "work for an example" in {
      val input =
        """....XXMAS.
          |.SAMXMS...
          |...S..A...
          |..A.A.MS.X
          |XMASAMX.MM
          |X.....XA.A
          |S.S.S.S.SS
          |.A.A.A.A.A
          |..M.M.M.MM
          |.X.X.XMASX""".stripMargin
      new Four().part1(input) shouldEqual 18
    }
  }

  "cross match" should {
    "works for an example" in {
      val input = """M.S
                   |.A.
                   |M.S""".stripMargin
      new Four().crossMatch1(
        input.split("\n").map(_.trim.toCharArray),
        0,
        0
      ) shouldEqual true
    }
  }

  "part2" should {
    "work for an example" in {
      val input =
        """MMMSXXMASM
          |MSAMXMSMSA
          |AMXSXMAAMM
          |MSAMASMSMX
          |XMASAMXAMM
          |XXAMMXXAMA
          |SMSMSASXSS
          |SAXAMASAAA
          |MAMMMXMMMM
          |MXMXAXMASX""".stripMargin
      new Four().part2(input) shouldEqual 9
    }
  }


  "solve" should {
    "work for an example" in {
      val input =
        """MMMSXXMASM
          |MSAMXMSMSA
          |AMXSXMAAMM
          |MSAMASMSMX
          |XMASAMXAMM
          |XXAMMXXAMA
          |SMSMSASXSS
          |SAXAMASAAA
          |MAMMMXMMMM
          |MXMXAXMASX""".stripMargin
      new Four().part2(input) shouldEqual 9
    }
  }
}
