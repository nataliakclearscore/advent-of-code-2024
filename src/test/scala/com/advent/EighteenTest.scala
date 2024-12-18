package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class EighteenTest extends AnyWordSpec with should.Matchers {

  private val underTest = new Eighteen

  "example" should {
    val input =
      """5,4
        |4,2
        |4,5
        |3,0
        |2,1
        |6,3
        |2,4
        |1,5
        |0,6
        |3,3
        |2,6
        |5,1
        |1,2
        |5,5
        |2,5
        |6,5
        |1,4
        |0,4
        |6,4
        |1,1
        |6,1
        |1,0
        |0,5
        |1,6
        |2,0""".stripMargin

    val list = input
      .split("\n")
      .map(_.trim)
      .map(_.split(","))
      .map(arr => (arr(0).toInt, arr(1).toInt))
      .toList

    "part 1" in {
      underTest.part1(list, 7, 12) shouldEqual 22
    }

    "part 2" in {
      underTest.part2(list, 7) shouldEqual (6, 1)
    }
  }
}
