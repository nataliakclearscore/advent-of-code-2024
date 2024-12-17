package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SeventeenTest extends AnyWordSpec with should.Matchers {

  def parse(input: String): (Int, Int, Int, List[Int]) = {
    val lines = input.split("\n").map(_.trim).filter(_.nonEmpty)

    def parseRegister(name: String): Int =
      lines
        .find(_.startsWith(s"Register $name:"))
        .map(_.split(":")(1).trim.toInt)
        .getOrElse(0)

    val a = parseRegister("A")
    val b = parseRegister("B")
    val c = parseRegister("C")

    val programLine = lines.find(_.startsWith("Program:")).getOrElse("")
    val programStr = programLine.split(":", 2)(1).trim

    val program = programStr.split(",").map(_.toInt).toList

    (a, b, c, program)
  }

  val underTest = new Seventeen

  "operands" should {
    "adv instruction (opcode 0)" in {
      underTest.process(
        100,
        0,
        0,
        List(0, 2)
      ) shouldEqual (25, 0, 0, List.empty)
      underTest.process(
        100,
        0,
        0,
        List(0, 3)
      ) shouldEqual (12, 0, 0, List.empty)
      underTest.process(
        100,
        2,
        0,
        List(0, 5)
      ) shouldEqual (25, 2, 0, List.empty)
      underTest.process(
        100,
        6,
        0,
        List(0, 5)
      ) shouldEqual (1, 6, 0, List.empty)
      underTest.process(100, 1, 2, List(0, 4)) shouldEqual (0, 1, 2, List.empty)
    }

    "bxl instruction (opcode 1)" in {
      underTest.process(0, 2, 0, List(1, 3)) shouldEqual (0, 1, 0, List.empty)
      underTest.process(0, 2, 0, List(1, 5)) shouldEqual (0, 7, 0, List.empty)
    }

    "bst instruction (opcode 2)" in {
      underTest.process(0, 0, 0, List(1, 2)) shouldEqual (0, 2, 0, List.empty)
      underTest.process(
        1000,
        0,
        0,
        List(1, 4)
      ) shouldEqual (1000, 4, 0, List.empty)
    }
  }

  "example 1" should {
    "part 1" in {
      val input =
        """Register A: 729
          |Register B: 0
          |Register C: 0
          |
          |Program: 0,1,5,4,3,0
          |""".stripMargin
      val (a, b, c, program) = parse(input)
      underTest.part1(a, b, c, program) shouldEqual "4,6,3,5,6,3,5,2,1,0"
    }
  }

  "example 2" should {
    "part 1 2024" in {
      val input =
        """Register A: 2024
          |Register B: 0
          |Register C: 0
          |
          |Program: 0,3,5,4,3,0
          |""".stripMargin
      val (a, b, c, program) = parse(input)
      underTest.part1(a, b, c, program) shouldEqual "5,7,3,0"
    }

    "part 1 117440" in {
      val input =
        """Register A: 117440
          |Register B: 0
          |Register C: 0
          |
          |Program: 0,3,5,4,3,0
          |""".stripMargin
      val (a, b, c, program) = parse(input)
      underTest.part1(a, b, c, program) shouldEqual "0,3,5,4,3,0"
    }
  }

  "solve" should {
    val input =
      """Register A: 37283687
        |Register B: 0
        |Register C: 0
        |
        |Program: 2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0""".stripMargin

    "part 1" in {
      val (a, b, c, program) = parse(input)
      underTest.part1(a, b, c, program) shouldEqual "1,5,3,0,2,5,2,5,3"
    }

    "part 2 exploring" in {
      val (a, b, c, program) = parse(input)
      for (i <- 1 to 100) {
        val res = underTest.part1(i, 0, 0, program)
        println(s"$i: $res")
      }

      // Noticed: the output reminds me of the octal number system

      underTest.part1(3, b, c, program) shouldEqual "0"

      underTest.part1(24, b, c, program) shouldEqual "3,0"

      underTest.part1(
        underTest.octalToDecimal("3045").toInt,
        b,
        c,
        program
      ) shouldEqual "5,5,3,0"

      underTest.part1(
        underTest.octalToDecimal("304513010").toLong,
        b,
        c,
        program
      ) shouldEqual "1,1,3,0,3,5,5,3,0"

      underTest.part1(
        underTest.octalToDecimal("30451301045"),
        b,
        c,
        program
      ) shouldEqual "5,4,1,1,3,0,3,5,5,3,0"
      
      // Noticed: adding numbers to the end of the octal number adds them to the beginning of the output
    }

    "part2" in {
      val (a, b, c, program) = parse(input)
      underTest.part2(
        program
      ) shouldEqual BigInt("108107566389757")
    }

  }
}
