package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwentyTest extends AnyWordSpec with should.Matchers {

  val underTest = new Twenty

  "example 1" should {

    val input =
      """###############
        |#...#...#.....#
        |#.#.#.#.#.###.#
        |#S#...#.#.#...#
        |#######.#.#.###
        |#######.#.#...#
        |#######.#.###.#
        |###..E#...#...#
        |###.#######.###
        |#...###...#...#
        |#.#####.#.###.#
        |#.#...#.#.#...#
        |#.#.#.#.#.#.###
        |#...#...#...###
        |###############""".stripMargin

    "part 1" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      val savings = underTest.findSavingsPart1(grid)
      savings(64) shouldBe 1
      savings(4) shouldBe 14
    }

    "part 2" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      val savings = underTest.findSavingsPart2(grid, minSaved = 50)
      savings(50) shouldBe 32
      savings(76) shouldBe 3
    }
  }
}
