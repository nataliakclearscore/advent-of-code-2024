package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ElevenTest extends AnyWordSpec with should.Matchers {

  private val underTest = Eleven()

  "blink" should {
    "work for an example" in {
      val l0 = List(125, 17).map(_.toLong)
      val l1 = underTest.blink(l0)
      l1 shouldEqual List(253000, 1, 7).map(_.toLong)
      val l2 = underTest.blink(l1)
      l2 shouldEqual List(253, 0, 2024, 14168).map(_.toLong)
      val l3 = underTest.blink(l2)
      l3 shouldEqual List(512072, 1, 20, 24, 28676032).map(_.toLong)
      val l4 = underTest.blink(l3)
      val l5 = underTest.blink(l4)
      val l6 = underTest.blink(l5)
      l6 shouldEqual List(2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40,
        48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2).map(_.toLong)
    }
  }

  "blinkN and and blinkNRec part1" should {
    "work for an example" in {
      val l0 = List(125, 17).map(_.toLong)
      underTest.blinkN(l0, 6) shouldEqual 22
      underTest.blinkN(l0, 25) shouldEqual 55312
      underTest.part1("125 17") shouldEqual 55312
    }
  }
}
