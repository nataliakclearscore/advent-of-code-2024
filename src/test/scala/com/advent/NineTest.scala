package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class NineTest extends AnyWordSpec with should.Matchers {

  private val underTest = new Nine()

  private def toString(blocks: Array[Either[Char, Int]]): String =
    blocks.foldLeft("")((acc: String, block: Either[Char, Int]) =>
      acc + block.fold(_.toString, _.toString)
    )

  "toBlocks" should {
    "work examples" in {
      toString(underTest.toBlocks("12345")) shouldBe "0..111....22222"
      toString(
        underTest.toBlocks("90909")
      ) shouldBe "000000000111111111222222222"
      toString(
        underTest
          .toBlocks(
            "2333133121414131402"
          )
      ) shouldBe "00...111...2...333.44.5555.6666.777.888899"
      toString(
        underTest
          .toBlocks(
            "233313312141413140212"
          )
      ) shouldBe "00...111...2...333.44.5555.6666.777.888899.1010"
      var expected: Array[Either[Char, Int]] = Array(
        Right(0),
        Right(0),
        Left('.'),
        Left('.'),
        Left('.'),
        Right(1),
        Right(1),
        Right(1),
        Left('.'),
        Left('.'),
        Left('.'),
        Right(2),
        Left('.'),
        Left('.'),
        Left('.'),
        Right(3),
        Right(3),
        Right(3),
        Left('.'),
        Right(4),
        Right(4),
        Left('.'),
        Right(5),
        Right(5),
        Right(5),
        Right(5),
        Left('.'),
        Right(6),
        Right(6),
        Right(6),
        Right(6),
        Left('.'),
        Right(7),
        Right(7),
        Right(7),
        Left('.'),
        Right(8),
        Right(8),
        Right(8),
        Right(8),
        Right(9),
        Right(9),
        Left('.'),
        Right(10),
        Right(10)
      )
      underTest.toBlocks("233313312141413140212") shouldBe expected
    }
  }

  "compact" should {
    "work examples" in {
      toString(
        underTest.compact(underTest.toBlocks("12345"))
      ) shouldBe "022111222......"
      toString(
        underTest.compact(
          underTest
            .toBlocks(
              "2333133121414131402"
            )
        )
      ) shouldBe "0099811188827773336446555566.............."
    }
  }

  "part1" should {
    "give checksums for examples" in {
      underTest.part1(
        "2333133121414131402"
      ) shouldBe 1928
    }
  }
}
