package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class NineTest extends AnyWordSpec with should.Matchers {

  private val underTest = new Nine()

  private def toString(blocks: Array[Either[Char, Int]]): String =
    blocks.foldLeft("")((acc: String, block: Either[Char, Int]) =>
      acc + block.fold(_.toString, _.toString)
    )

  "toBlocks1" should {
    "work examples" in {
      toString(underTest.toBlocks1("12345")) shouldBe "0..111....22222"
      toString(
        underTest.toBlocks1("90909")
      ) shouldBe "000000000111111111222222222"
      toString(
        underTest
          .toBlocks1(
            "2333133121414131402"
          )
      ) shouldBe "00...111...2...333.44.5555.6666.777.888899"
      toString(
        underTest
          .toBlocks1(
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
      underTest.toBlocks1("233313312141413140212") shouldBe expected
    }
  }

  "compact" should {
    "work examples" in {
      toString(
        underTest.compact1(underTest.toBlocks1("12345"))
      ) shouldBe "022111222......"
      toString(
        underTest.compact1(
          underTest
            .toBlocks1(
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

  "toBlocks2" should {
    "work for 12345" in {
      val res = underTest.toBlocks2("12345")
      toString(res.blocks) shouldBe "0..111....22222"
      res.fileBlocks shouldBe Array(
        DiskBlock(Right(0), 0, 1),
        DiskBlock(Right(1), 3, 3),
        DiskBlock(Right(2), 10, 5)
      )
      res.spaceBlocks shouldBe Array(
        DiskBlock(Left('.'), 1, 2),
        DiskBlock(Left('.'), 6, 4)
      )
    }

    "work for 2333133121414131402" in {
      toString(
        underTest.toBlocks2("90909").blocks
      ) shouldBe "000000000111111111222222222"
      toString(
        underTest
          .toBlocks2(
            "2333133121414131402"
          )
          .blocks
      ) shouldBe "00...111...2...333.44.5555.6666.777.888899"
      toString(
        underTest
          .toBlocks2(
            "233313312141413140212"
          )
          .blocks
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
      underTest.toBlocks2("233313312141413140212").blocks shouldBe expected
    }
  }

  "compact2" should {
    "work for 2333133121414131402" in {
      toString(
        underTest.compact2(
          underTest
            .toBlocks2(
              "2333133121414131402"
            )
        )
      ) shouldBe "00992111777.44.333....5555.6666.....8888.."
    }
  }

  "part2" should {
    "give checksums for examples" in {
      underTest.part2(
        "2333133121414131402"
      ) shouldBe 2858
    }
  }
}
