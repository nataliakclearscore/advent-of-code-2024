package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source
import scala.util.Using

class TwentyFiveTest extends AnyWordSpec with should.Matchers {

  val underTest = new TwentyFive

  "examlpe" should {
    val input = """#####
                  |.####
                  |.####
                  |.####
                  |.#.#.
                  |.#...
                  |.....
                  |
                  |#####
                  |##.##
                  |.#.##
                  |...##
                  |...#.
                  |...#.
                  |.....
                  |
                  |.....
                  |#....
                  |#....
                  |#...#
                  |#.#.#
                  |#.###
                  |#####
                  |
                  |.....
                  |.....
                  |#.#..
                  |###..
                  |###.#
                  |###.#
                  |#####
                  |
                  |.....
                  |.....
                  |.....
                  |#....
                  |#.#..
                  |#.#.#
                  |#####""".stripMargin
    "parse" in {
      underTest.parse(input) shouldEqual
        (List(Lock(List(0, 5, 3, 4, 3)), Lock(List(1, 2, 0, 5, 3))),
        List(
          Key(List(5, 0, 2, 1, 3)),
          Key(List(4, 3, 4, 0, 2)),
          Key(List(3, 0, 2, 0, 1))
        ))
    }

    "fits" in {
      underTest.fits(
        Lock(List(0, 5, 3, 4, 3)),
        Key(List(5, 0, 2, 1, 3))
      ) shouldBe false
      underTest.fits(
        Lock(List(0, 5, 3, 4, 3)),
        Key(List(4, 3, 4, 0, 2))
      ) shouldBe false
      underTest.fits(
        Lock(List(0, 5, 3, 4, 3)),
        Key(List(3, 0, 2, 0, 1))
      ) shouldBe true

      underTest.fits(
        Lock(List(1, 2, 0, 5, 3)),
        Key(List(5, 0, 2, 1, 3))
      ) shouldBe false
      underTest.fits(
        Lock(List(1, 2, 0, 5, 3)),
        Key(List(4, 3, 4, 0, 2))
      ) shouldBe true
      underTest.fits(
        Lock(List(1, 2, 0, 5, 3)),
        Key(List(3, 0, 2, 0, 1))
      ) shouldBe true
    }

    "part 1" in {
      val (locks, keys) = underTest.parse(input)
      underTest.part1(locks, keys) shouldEqual 3
    }
  }
}
