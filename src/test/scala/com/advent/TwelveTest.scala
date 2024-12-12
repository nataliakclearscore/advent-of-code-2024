package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwelveTest extends AnyWordSpec with should.Matchers {

  private val underTest = new Twelve

  "buildRegion" should {
    "can be used for area calculation ex 1" in {
      val input =
        """AAAA
          |BBCD
          |BBCC
          |EEEC""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      // valids
      underTest
        .buildRegion(map, 'A', (0, 0), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 4
      underTest
        .buildRegion(map, 'B', (1, 0), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 4
      underTest
        .buildRegion(map, 'C', (1, 2), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 4
      underTest
        .buildRegion(map, 'E', (3, 0), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 3
      underTest
        .buildRegion(map, 'D', (1, 3), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 1

      // invalids
      underTest
        .buildRegion(map, 'B', (0, 0), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 0
      underTest
        .buildRegion(map, 'A', (1, 0), Array.ofDim[Boolean](4, 4))
        .size shouldEqual 0
    }
  }

  "calcPerimeter" should {
    "work for ex 1" in {
      val input =
        """AAAA
          |BBCD
          |BBCC
          |EEEC""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      // valids
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'A', (0, 0), Array.ofDim[Boolean](4, 4)),
        'A'
      ) shouldEqual 10
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'B', (1, 0), Array.ofDim[Boolean](4, 4)),
        'B'
      ) shouldEqual 8
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'C', (1, 2), Array.ofDim[Boolean](4, 4)),
        'C'
      ) shouldEqual 10
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'E', (3, 0), Array.ofDim[Boolean](4, 4)),
        'E'
      ) shouldEqual 8
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'D', (1, 3), Array.ofDim[Boolean](4, 4)),
        'D'
      ) shouldEqual 4
    }

    "work for ex 2" in {
      val input =
        """OOOOO
          |OXOXO
          |OOOOO
          |OXOXO
          |OOOOO""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      underTest.calcPerimeter(
        map,
        underTest.buildRegion(map, 'O', (0, 0), Array.ofDim[Boolean](5, 5)),
        'O'
      ) shouldEqual 36
    }
  }

  "part 1" should {
    "work for an example 1" in {
      val input =
        """AAAA
          |BBCD
          |BBCC
          |EEEC""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      underTest.part1(map) shouldEqual 140
    }

    "work for an example 2" in {
      val input =
        """OOOOO
          |OXOXO
          |OOOOO
          |OXOXO
          |OOOOO""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      underTest.part1(map) shouldEqual 772
    }

    "work for an example 3" in {
      val input =
        """RRRRIICCFF
          |RRRRIICCCF
          |VVRRRCCFFF
          |VVRCCCJFFF
          |VVVVCJJCFE
          |VVIVCCJJEE
          |VVIIICJJEE
          |MIIIIIJJEE
          |MIIISIJEEE
          |MMMISSJEEE""".stripMargin
      val map: Array[Array[Char]] =
        input.split("\n").map(_.trim.toCharArray)
      underTest.part1(map) shouldEqual 1930
    }
  }
}
