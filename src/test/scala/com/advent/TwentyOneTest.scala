package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwentyOneTest extends AnyWordSpec with should.Matchers {

  private val underTest = new TwentyOne

  "test" should {
    "operate numeric keypad" in {
      underTest
        .pressButtonWays(underTest.numericGrid, 'A', '0') shouldBe List(
        List('<', 'A')
      )
      underTest
        .pressButtonWays(underTest.numericGrid, '0', '2') shouldBe List(
        List('^', 'A')
      )
      underTest
        .pressButtonWays(underTest.numericGrid, '2', '9') shouldBe List(
        List('^', '^', '>', 'A'),
        List('>', '^', '^', 'A')
      )
      underTest
        .pressButtonWays(underTest.numericGrid, '9', 'A') shouldBe List(
        List('v', 'v', 'v', 'A')
      )
    }

    "operate numeric keypad multi buttons" in {
      val res = underTest.pressButtonsWays(
        underTest.numericGrid,
        List('0', '2', '9', 'A')
      )
      res.size shouldBe 2
      res should contain(
        List('<', 'A', '^', 'A', '^', '^', '>', 'A', 'v', 'v', 'v', 'A')
      )
    }

    "operate directional keypad" in {
      underTest
        .pressButtonWays(underTest.directionalGrid, 'A', '^')
        .toList shouldBe List(List('<', 'A'))
      underTest
        .pressButtonWays(underTest.directionalGrid, '<', 'A')
        .toList shouldBe List(List('>', '>', '^', 'A'))
    }

    "operate numeric keypad multi layers 1" in {
      underTest
        .pressButtonsAllLayers(List('0', '2', '9', 'A'), 2)
        .size shouldBe 68
      underTest
        .pressButtonsAllLayers(List('9', '8', '0', 'A'), 2)
        .size shouldBe 60
      underTest
        .pressButtonsAllLayers(List('1', '7', '9', 'A'), 2)
        .size shouldBe 68
      underTest
        .pressButtonsAllLayers(List('4', '5', '6', 'A'), 2)
        .size shouldBe 64
    }

    "operate numeric keypad multi layers 2" in {
      underTest
        .pressButtonsAllLayers(List('3', '7', '9', 'A'), 2)
        .size shouldBe 64
    }

    "part 1" in {
      val input =
        """029A
          |980A
          |179A
          |456A
          |379A""".stripMargin
      underTest.part1(input.split("\n").toList, 2) shouldBe 126384
      underTest.part2(input.split("\n").toList, 2) shouldBe 126384
    }

    "core moves" in {
      val coreMoves = List(
        "<^A",
        "^<A",
        "<A",
        "<vA",
        "v<A",
        "^A",
        "vA",
        "^>A",
        ">^A",
        ">A",
        "v>A",
        ">vA"
      )
      coreMoves.foreach(move => {
        val options = underTest
          .pressButtonsWays(underTest.directionalGrid, move.toCharArray.toList)
          .flatMap(level1 =>
            underTest
              .pressButtonsWays(underTest.directionalGrid, level1)
              .map(level2 => (level1.mkString, level2.mkString))
          ).flatMap(level2 =>
            underTest
              .pressButtonsWays(underTest.directionalGrid, level2._2.toCharArray.toList)
              .map(level3 => (level2._1, level2._2, level3.mkString))
          )
        val res = options.minBy(_._3.length)
        println(s"""case "$move" => "${res._1}" """)
      })
    }
  }

  "solve" should {
    "part 1" in {
      val input =
        """805A
          |170A
          |129A
          |283A
          |540A""".stripMargin
      underTest.part1(input.split("\n").toList, 2) shouldBe 137870
      underTest.part2(input.split("\n").toList, 2) shouldBe 137870
    }

    "level 3 comparison" in {
      val input = "1A"
      val ways = underTest
        .pressButtonsAllLayers(
          input.toCharArray.toList,
          3
        )
        .mkString
      println(s"ways: $ways")
      println(s"ways length: ${ways.length}")
      underTest.part2(input.split("\n").toList, 3) shouldBe underTest.part1(
        input.split("\n").toList,
        3
      )
    }

    "part 2" in {
      val input =
        """805A
          |170A
          |129A
          |283A
          |540A""".stripMargin
      underTest.part2(
        input.split("\n").toList,
        25
      ) shouldBe 170279148659464L
    }
  }
}
