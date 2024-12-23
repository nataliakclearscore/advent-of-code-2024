package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwentyThreeTest extends AnyWordSpec with should.Matchers {

  private def parse(input: String) = {
    input
      .split("\n")
      .map(_.trim)
      .map(_.split("-"))
      .map(arr => (arr(0), arr(1)))
      .toList
  }

  private val underTest = new TwentyThree

  "examples" should {
    val input =
      """kh-tc
        |qp-kh
        |de-cg
        |ka-co
        |yn-aq
        |qp-ub
        |cg-tb
        |vc-aq
        |tb-ka
        |wh-tc
        |yn-cg
        |kh-ub
        |ta-co
        |de-co
        |tc-td
        |tb-wq
        |wh-td
        |ta-ka
        |td-qp
        |aq-cg
        |wq-ub
        |ub-vc
        |de-ta
        |wq-aq
        |wq-vc
        |wh-yn
        |ka-de
        |kh-ta
        |co-tc
        |wh-qp
        |tb-vc
        |td-yn""".stripMargin

    "test findTriples" in {
      val pairs = parse(input)
      underTest.findTriples(pairs).size shouldEqual 12
    }

    "test part1" in {
      val pairs = parse(input)
      underTest.part1(pairs) shouldEqual 7
    }

    "test part2" in {
      val pairs = parse(input)
      underTest.part2(pairs) shouldEqual "co,de,ka,ta"
    }
  }

}
