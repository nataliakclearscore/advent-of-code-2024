package com.advent

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TwoTest extends AnyFlatSpec with should.Matchers {
  "part1Lists" should "work for an example" in {
    val input =
      """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".stripMargin
    new Two().part1Lists(input) shouldEqual List(true, false, false, false,
      false, true)
  }

  "part1" should "work for an example" in {
    val input =
      """7 6 4 2 1
  1 2 7 8 9
  9 7 6 2 1
  1 3 2 4 5
  8 6 4 4 1
  1 3 6 7 9""".stripMargin
    new Two().part1(input) shouldEqual 2
  }
}
