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
      val savings = underTest.findSavings(grid)
      savings(64) shouldBe 1
      savings(4) shouldBe 14
    }
  }
}
