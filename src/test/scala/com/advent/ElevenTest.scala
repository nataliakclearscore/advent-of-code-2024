package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ElevenTest extends AnyWordSpec with should.Matchers {

  private val underTest = Eleven()

  "blink1" should {
    "work for an example" in {
      val l0 = List(125, 17).map(_.toLong)
      val l1 = underTest.blink1(l0)
      l1 shouldEqual List(253000, 1, 7).map(_.toLong)
      val l2 = underTest.blink1(l1)
      l2 shouldEqual List(253, 0, 2024, 14168).map(_.toLong)
      val l3 = underTest.blink1(l2)
      l3 shouldEqual List(512072, 1, 20, 24, 28676032).map(_.toLong)
      val l4 = underTest.blink1(l3)
      val l5 = underTest.blink1(l4)
      val l6 = underTest.blink1(l5)
      l6 shouldEqual List(2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40,
        48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2).map(_.toLong)
    }
  }

  "blinkN1 and part1" should {
    "work for an example" in {
      val l0 = List(125, 17).map(_.toLong)
      underTest.blinkN1(l0, 6) shouldEqual 22
      underTest.blinkN1(l0, 25) shouldEqual 55312
      underTest.part1("125 17") shouldEqual 55312
    }
  }

  "blinkN2" should {
    "work for an example 1" in {
      val l0 = List(125, 17).map(_.toLong)
      underTest.blinkN2(l0, 6) shouldEqual 22
    }

    "work for an example 2" in {
      val l0 = List(125, 17).map(_.toLong)
      underTest.blinkN2(l0, 25) shouldEqual 55312
    }
  }

  "part2" should {
    "work for an example" in {
      underTest.part2("125 17") shouldEqual 65601038650482L
    }
  }
}
