package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

import scala.util.matching.Regex

class TwentyFourTest extends AnyWordSpec with should.Matchers {

  private def parse(input: String) = {
    val lines: List[String] = input.split("\\r?\\n").toList

    val valuePattern: Regex = """^([xy]\d{2}):\s*(\d+)$""".r

    val instructionPattern: Regex =
      """^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s*->\s*(\w+)$""".r

    val assignments: Map[String, Int] = lines.collect {
      case valuePattern(varName, valueStr) =>
        varName -> valueStr.toInt
    }.toMap

    val instructions: List[(String, String, String, String)] = lines.collect {
      case instructionPattern(lhs, op, rhs, out) =>
        (lhs, op, rhs, out)
    }

    (assignments, instructions)
  }

  private val underTest = new TwentyFour

  "test" should {

    "operations" in {
      1 & 1 shouldEqual 1
      1 & 0 shouldEqual 0
      0 & 1 shouldEqual 0
      0 & 0 shouldEqual 0

      1 | 1 shouldEqual 1
      1 | 0 shouldEqual 1
      0 | 1 shouldEqual 1
      0 | 0 shouldEqual 0

      1 ^ 1 shouldEqual 0
      1 ^ 0 shouldEqual 1
      0 ^ 1 shouldEqual 1
      0 ^ 0 shouldEqual 0
    }
  }

  "example 1" should {

    val input = """x00: 1
              |x01: 1
              |x02: 1
              |y00: 0
              |y01: 1
              |y02: 0
              |
              |x00 AND y00 -> z00
              |x01 XOR y01 -> z01
              |x02 OR y02 -> z02""".stripMargin

    "find z values" in {
      val (assignments, instructions) = parse(input)
      val result =
        underTest.findZValues(assignments, instructions) shouldBe List(1, 0, 0)
    }

    "part 1" in {
      val (assignments, instructions) = parse(input)
      val result = underTest.part1(assignments, instructions) shouldBe 4
    }
  }

  "example 2" should {

    val input =
      """x00: 1
        |x01: 0
        |x02: 1
        |x03: 1
        |x04: 0
        |y00: 1
        |y01: 1
        |y02: 1
        |y03: 1
        |y04: 1
        |
        |ntg XOR fgs -> mjb
        |y02 OR x01 -> tnw
        |kwq OR kpj -> z05
        |x00 OR x03 -> fst
        |tgd XOR rvg -> z01
        |vdt OR tnw -> bfw
        |bfw AND frj -> z10
        |ffh OR nrd -> bqk
        |y00 AND y03 -> djm
        |y03 OR y00 -> psh
        |bqk OR frj -> z08
        |tnw OR fst -> frj
        |gnj AND tgd -> z11
        |bfw XOR mjb -> z00
        |x03 OR x00 -> vdt
        |gnj AND wpb -> z02
        |x04 AND y00 -> kjc
        |djm OR pbm -> qhw
        |nrd AND vdt -> hwm
        |kjc AND fst -> rvg
        |y04 OR y02 -> fgs
        |y01 AND x02 -> pbm
        |ntg OR kjc -> kwq
        |psh XOR fgs -> tgd
        |qhw XOR tgd -> z09
        |pbm OR djm -> kpj
        |x03 XOR y03 -> ffh
        |x00 XOR y04 -> ntg
        |bfw OR bqk -> z06
        |nrd XOR fgs -> wpb
        |frj XOR qhw -> z04
        |bqk OR frj -> z07
        |y03 OR x01 -> nrd
        |hwm AND bqk -> z03
        |tgd XOR rvg -> z12
        |tnw OR pbm -> gnj""".stripMargin

    "find z values" in {
      val (assignments, instructions) = parse(input)
      val result =
        underTest.findZValues(assignments, instructions) shouldBe List(0, 0, 1,
          1, 1, 1, 1, 1, 0, 1, 0, 0, 0)
    }

    "part 1" in {
      val (assignments, instructions) = parse(input)
      val result = underTest.part1(assignments, instructions) shouldBe 2024
    }
  }
}
