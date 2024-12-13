package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ThirteenTest extends AnyWordSpec with should.Matchers {

  private def parse(
      lines: Array[String]
  ): List[((Long, Long), (Long, Long), (Long, Long))] = {
    val buttonAPattern = """Button A: X\+(\d+), Y\+(\d+)""".r
    val buttonBPattern = """Button B: X\+(\d+), Y\+(\d+)""".r
    val prizePattern = """Prize: X=(\d+), Y=(\d+)""".r
    lines
      .filter(_.trim.nonEmpty)
      .grouped(3)
      .toList
      .map(arr => {
        var buttonA: (Long, Long) = (0, 0)
        var buttonB: (Long, Long) = (0, 0)
        var prize: (Long, Long) = (0, 0)

        arr.foreach {
          case buttonAPattern(x, y) =>
            buttonA = (x.toLong, y.toLong)
          case buttonBPattern(x, y) =>
            buttonB = (x.toLong, y.toLong)
          case prizePattern(x, y) =>
            prize = (x.toLong, y.toLong)
          case _ => // Ignore other lines
        }
        (buttonA, buttonB, prize)
      })
  }

  private val underTest = new Thirteen

  "part1Lines" should {
    "can be used for area calculation ex 1" in {
      val input =
        """Button A: X+94, Y+34
          |Button B: X+22, Y+67
          |Prize: X=8400, Y=5400
          |
          |Button A: X+26, Y+66
          |Button B: X+67, Y+21
          |Prize: X=12748, Y=12176
          |
          |Button A: X+17, Y+86
          |Button B: X+84, Y+37
          |Prize: X=7870, Y=6450
          |
          |Button A: X+69, Y+23
          |Button B: X+27, Y+71
          |Prize: X=18641, Y=10279""".stripMargin
      val lines: Array[String] = input.split("\n")
      val parsed = parse(lines)
      underTest.part1Line(
        parsed(0)._1,
        parsed(0)._2,
        parsed(0)._3
      ) shouldEqual Some((80, 40))
      underTest.part1Line(
        parsed(1)._1,
        parsed(1)._2,
        parsed(1)._3
      ) shouldEqual None
      underTest.part1Line(
        parsed(2)._1,
        parsed(2)._2,
        parsed(2)._3
      ) shouldEqual Some((38, 86))
      underTest.part1Line(
        parsed(3)._1,
        parsed(3)._2,
        parsed(3)._3
      ) shouldEqual None
    }
  }

  "part1" should {
    "can be used for area calculation ex 1" in {
      val input =
        """Button A: X+94, Y+34
          |Button B: X+22, Y+67
          |Prize: X=8400, Y=5400
          |
          |Button A: X+26, Y+66
          |Button B: X+67, Y+21
          |Prize: X=12748, Y=12176
          |
          |Button A: X+17, Y+86
          |Button B: X+84, Y+37
          |Prize: X=7870, Y=6450
          |
          |Button A: X+69, Y+23
          |Button B: X+27, Y+71
          |Prize: X=18641, Y=10279""".stripMargin
      val lines: Array[String] = input.split("\n")
      val parsed = parse(lines)
      underTest.part1(parsed) shouldEqual 480
    }
  }

  "part2" should {
    "deal with large numbers" in {
      underTest.part2(
        List(((20L, 57L), (59L, 21L), (10072L, 16868L)))
      ) shouldEqual 513081890108L
    }

    "can be used for area calculation ex 1" in {
      val input =
        """Button A: X+94, Y+34
          |Button B: X+22, Y+67
          |Prize: X=8400, Y=5400
          |
          |Button A: X+26, Y+66
          |Button B: X+67, Y+21
          |Prize: X=12748, Y=12176
          |
          |Button A: X+17, Y+86
          |Button B: X+84, Y+37
          |Prize: X=7870, Y=6450
          |
          |Button A: X+69, Y+23
          |Button B: X+27, Y+71
          |Prize: X=18641, Y=10279""".stripMargin
      val lines: Array[String] = input.split("\n")
      val parsed = parse(lines)
      underTest.part1Line(
        parsed(0)._1,
        parsed(0)._2,
        (parsed(0)._3._1 + 10000000000000L, parsed(0)._3._2 + 10000000000000L)
      ) shouldEqual None
      underTest.part1Line(
        parsed(1)._1,
        parsed(1)._2,
        (parsed(1)._3._1 + 10000000000000L, parsed(1)._3._2 + 10000000000000L)
      ) shouldBe a[Some[(BigInt, BigInt)]]
      underTest.part1Line(
        parsed(2)._1,
        parsed(2)._2,
        (parsed(2)._3._1 + 10000000000000L, parsed(2)._3._2 + 10000000000000L)
      ) shouldEqual None
      underTest.part1Line(
        parsed(3)._1,
        parsed(3)._2,
        (parsed(3)._3._1 + 10000000000000L, parsed(3)._3._2 + 10000000000000L)
      ) shouldBe a[Some[(BigInt, BigInt)]]
    }
  }
}
