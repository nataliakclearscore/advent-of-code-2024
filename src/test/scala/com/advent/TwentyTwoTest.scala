package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TwentyTwoTest extends AnyWordSpec with should.Matchers {

  private val underTest = new TwentyTwo

  "examples part 1" should {
    "test mix" in {
      underTest.mix(42, 15) shouldBe 37
    }

    "test prune" in {
      underTest.prune(100000000L) shouldBe 16113920L
    }

    "nextSecret" in {
      underTest.nextSecret(123L) shouldBe 15887950L
      underTest.nextSecretN(123L, 1) shouldBe 15887950L
      underTest.nextSecretN(123L, 2) shouldBe 16495136L
      underTest.nextSecretN(123L, 3) shouldBe 527345L
      underTest.nextSecretN(123L, 4) shouldBe 704524L
      underTest.nextSecretN(123L, 5) shouldBe 1553684L
      underTest.nextSecretN(123L, 6) shouldBe 12683156L
      underTest.nextSecretN(123L, 7) shouldBe 11100544L
      underTest.nextSecretN(123L, 8) shouldBe 12249484L
      underTest.nextSecretN(123L, 9) shouldBe 7753432L
      underTest.nextSecretN(123L, 10) shouldBe 5908254L
    }

    "nextSecret 2000" in {
      underTest.nextSecretN(1L, 2000) shouldBe 8685429L
      underTest.nextSecretN(10L, 2000) shouldBe 4700978L
      underTest.nextSecretN(100L, 2000) shouldBe 15273692L
      underTest.nextSecretN(2024L, 2000) shouldBe 8667524L
    }

    "part1" in {
      val input =
        """1
          |10
          |100
          |2024""".stripMargin
      val secrets = input.split("\n").map(_.trim.toLong).toList
      underTest.part1(secrets) shouldBe 37327623L
    }
  }

  "examples part 2" should {
    "sequences" in {
      val prices = underTest.priceSequence(123, 10)
      prices shouldBe List(3, 0, 6, 5, 4, 4, 6, 4, 4, 2)
      val changes = underTest.changesSequence(prices)
      changes shouldBe List(-3, 6, -1, -1, 0, 2, -2, 0, -2)
    }

    "buildSequenceMap" in {
      val prices = underTest.priceSequence(123, 10)
      underTest.buildSequenceMap(prices) shouldBe Map(
        "0,2,-2,0" -> 4,
        "2,-2,0,-2" -> 2,
        "6,-1,-1,0" -> 4,
        "-3,6,-1,-1" -> 4,
        "-1,0,2,-2" -> 4,
        "-1,-1,0,2" -> 6
      )
    }

    "buildSequenceMap 2" in {
      val prices = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      underTest.buildSequenceMap(prices) shouldBe Map("1,1,1,1" -> 5)
    }

    "buildSequenceMap 3" in {
      val prices = List(1, 2, 3, 4, 5, 6, 7, 9)
      underTest.buildSequenceMap(prices) shouldBe Map(
        "1,1,1,2" -> 9,
        "1,1,1,1" -> 5
      )
    }

    "bestPrice" in {
      val input =
        """1
          |2
          |3
          |2024""".stripMargin
      val secrets = input.split("\n").map(_.trim.toLong).toList
      underTest.part2(secrets) shouldBe 23
    }
  }
}
