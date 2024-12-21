package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwentyOneTest extends AnyWordSpec with should.Matchers {

  private val underTest = new TwentyOne

  "test" should {
    "operate numeric keypad" in {
      underTest
        .findButton(underTest.numericGrid, 'A', '0')
        .toList shouldBe List('<')
      underTest
        .findButton(underTest.numericGrid, '0', '2')
        .toList shouldBe List('^')
      underTest
        .findButton(underTest.numericGrid, '2', '9')
        .toList shouldBe List('^', '^', '>')
      underTest
        .findButton(underTest.numericGrid, '9', 'A')
        .toList shouldBe List('v', 'v', 'v')
    }

    "operate numeric keypad multi buttons" in {
      underTest.pressButtons(
        underTest.numericGrid,
        List('0', '2', '9', 'A')
      ) shouldBe List('<', 'A', '^', 'A', '^', '^', '>', 'A', 'v', 'v', 'v',
        'A')
    }

    "operate directional keypad" in {
      underTest
        .findButton(underTest.directionalGrid, 'A', '^')
        .toList shouldBe List('<')
      underTest
        .findButton(underTest.directionalGrid, 'v', 'A')
        .toList shouldBe List('^', '>')
    }

    "operate numeric keypad multi layers 1" in {
      underTest.pressButtonsAllLayers(List('0', '2', '9', 'A')).size shouldBe 68
      underTest.pressButtonsAllLayers(List('9', '8', '0', 'A')).size shouldBe 60
      underTest.pressButtonsAllLayers(List('1', '7', '9', 'A')).size shouldBe 68
      underTest.pressButtonsAllLayers(List('4', '5', '6', 'A')).size shouldBe 64
    }

    "operate numeric keypad multi layers 2" in {
      underTest.pressButtonsAllLayers(List('3', '7', '9', 'A')).size shouldBe 64
    }

    "part 1" in {
      val input =
        """029A
          |980A
          |179A
          |456A
          |379A""".stripMargin
      underTest.part1(input.split("\n").toList) shouldBe 126384
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
      underTest.part1(input.split("\n").toList) shouldBe 0 // high: 141542
    }
  }
}
