package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class SevenTest extends AnyWordSpec with should.Matchers {

  "part1" should {
    "work for an example" in {
      val input =
        """190: 10 19
          |3267: 81 40 27
          |83: 17 5
          |156: 15 6
          |7290: 6 8 6 15
          |161011: 16 10 13
          |192: 17 8 14
          |21037: 9 7 18 13
          |292: 11 6 16 20""".stripMargin
      val lines: Array[String] = input.split("\n")
      val inputs = lines.map(line => {
        val split = line.split(":")
        val res: Long = split(0).toLong
        val nums: List[Long] = split(1).trim.split(" ").map(_.toLong).toList
        (res, nums)
      })
      inputs.map((res, nums) =>
        new Seven().part1Line(res, nums)
      ) shouldEqual List(true, true, false, false, false, false, false, false,
        true)
      inputs
        .map((res, nums) => (res, new Seven().part1Line(res, nums)))
        .filter(_._2)
        .map(_._1)
        .sum shouldEqual 3749
    }
  }

  "part2" should {
    "combining numbers" in {
      val x: Long = 15;
      val y: Long = 6;
      (x.toString + y.toString).toLong shouldEqual 156

      val x1: Long = 1000000000
      val y1: Long = 1000000000
      Try((x1.toString + x1.toString).toLong).toOption shouldBe None
    }

    "work for an example" in {
      val input =
        """190: 10 19
          |3267: 81 40 27
          |83: 17 5
          |156: 15 6
          |7290: 6 8 6 15
          |161011: 16 10 13
          |192: 17 8 14
          |21037: 9 7 18 13
          |292: 11 6 16 20""".stripMargin
      val lines: Array[String] = input.split("\n")
      val inputs = lines.map(line => {
        val split = line.split(":")
        val res: BigInt = split(0).toLong
        val nums: List[BigInt] = split(1).trim.split(" ").map(BigInt(_)).toList
        (res, nums)
      })
      inputs.map((res, nums) =>
        new Seven().part2Line(res, nums)
      ) shouldEqual List(true, true, false, true, true, false, true, false,
        true)
      inputs
        .map((res, nums) => (res, new Seven().part2Line(res, nums)))
        .filter(_._2)
        .map(_._1)
        .sum shouldEqual 11387
    }

    "works for line" in {
      val line = "1874160190: 8 8 22 2 8 4 64 5 5 5 133"
      val split = line.split(":")
      val res: BigInt = split(0).toLong
      val nums: List[BigInt] = split(1).trim.split(" ").map(BigInt(_)).toList
      new Seven().part2Line(res, nums) shouldBe true
    }
  }
}
