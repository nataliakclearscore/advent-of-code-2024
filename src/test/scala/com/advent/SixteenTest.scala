package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SixteenTest extends AnyWordSpec with should.Matchers {

  val underTest = new Sixteen

  "example 1" should {

    val input =
      """###############
        |#.......#....E#
        |#.#.###.#.###.#
        |#.....#.#...#.#
        |#.###.#####.#.#
        |#.#.#.......#.#
        |#.#.#####.###.#
        |#...........#.#
        |###.#.#####.#.#
        |#...#.....#.#.#
        |#.#.#.###.#.#.#
        |#.....#...#.#.#
        |#.###.#.#.#.#.#
        |#S..#.....#...#
        |###############""".stripMargin

    "part 1" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      underTest.part1(grid) shouldEqual 7036
    }

    "part 2" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      underTest.part2(grid) shouldEqual 45
    }
  }

  "example 2" should {
    val input =
      """#################
        |#...#...#...#..E#
        |#.#.#.#.#.#.#.#.#
        |#.#.#.#...#...#.#
        |#.#.#.#.###.#.#.#
        |#...#.#.#.....#.#
        |#.#.#.#.#.#####.#
        |#.#...#.#.#.....#
        |#.#.#####.#.###.#
        |#.#.#.......#...#
        |#.#.###.#####.###
        |#.#.#...#.....#.#
        |#.#.#.#####.###.#
        |#.#.#.........#.#
        |#.#.#.#########.#
        |#S#.............#
        |#################""".stripMargin

    "part 1" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      underTest.part1(grid) shouldEqual 11048
    }

    "part 2" in {
      val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
      underTest.part2(grid) shouldEqual 64
    }
  }
}
