package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ThreeTest extends AnyWordSpec with should.Matchers {
  "parseNum" should {
    "return empty if no number" in {
      new Three().parseNum("abc", 0) shouldBe ""
      new Three().parseNum("abc", 1) shouldBe ""
      new Three().parseNum("abc", 2) shouldBe ""
      new Three().parseNum(".", 0) shouldBe ""
    }

    "return empty if no number in the index" in {
      new Three().parseNum("123abc4576", 3) shouldBe ""
    }

    "return empty if index too big" in {
      new Three().parseNum("abc", 10) shouldBe ""
    }

    "return the number starting from the beginning" in {
      new Three().parseNum("123", 0) shouldBe "123"
    }

    "return the number until the first non digit starting from the beginning" in {
      new Three().parseNum("123abc4576", 0) shouldBe "123"
    }

    "return the number starting from index" in {
      new Three().parseNum("123", 1) shouldBe "23"
    }

    "return the number until the first non digit starting from index" in {
      new Three().parseNum("123abc4576", 1) shouldBe "23"
    }
  }

  "parseMul" should {
    "return None if no match" in {
      new Three().parseMul("abc", 0) shouldBe None
      new Three().parseMul("mul(", 0) shouldBe None
      new Three().parseMul("mul(123", 0) shouldBe None
      new Three().parseMul("mul(123,", 0) shouldBe None
      new Three().parseMul("mul(123,456", 0) shouldBe None
      new Three().parseMul("mdul(123,456)", 0) shouldBe None
      new Three().parseMul("mdul(r123,456)", 0) shouldBe None
      new Three().parseMul("mul(12d3,456)", 0) shouldBe None
      new Three().parseMul("mdul(123f,456)", 0) shouldBe None
      new Three().parseMul("mdul(123456)", 0) shouldBe None
      new Three().parseMul("mdul(123,d456)", 0) shouldBe None
      new Three().parseMul("mdul(123,45f6)", 0) shouldBe None
      new Three().parseMul("mdul(123,456d)", 0) shouldBe None
      new Three().parseMul("mul(1,2", 0) shouldBe None
      new Three().parseMul("mul(6,9!)", 0) shouldBe None
      new Three().parseMul("?(12,34)", 0) shouldBe None
      new Three().parseMul("mul(7,8.)", 0) shouldBe None
      new Three().parseMul("mul(7.0,8)", 0) shouldBe None
    }

    "return numbers if match" in {
      new Three().parseMul("mul(7,8)", 0) shouldBe Some("7", "8")
      new Three().parseMul("mul(7,89)", 0) shouldBe Some("7", "89")
      new Three().parseMul("mul(123,456)", 0) shouldBe Some("123", "456")
      new Three().parseMul("mul(46,0)", 0) shouldBe Some("46", "0")
    }

    "work with indexes other than 0" in {
      val input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      new Three().parseMul(input, 1) shouldBe Some("2", "4")
    }
  }

  "parseAllPart1" should {
    "work for an example all valid" in {
      val input = "mul(7,8)mul(123,456)mul(46,0)"
      new Three().parseAllPart1(input) shouldEqual List(
        ("46", "0"),
        ("123", "456"),
        ("7", "8")
      )
    }

    "work for an example" in {
      val input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      new Three().parseAllPart1(input) shouldEqual List(
        ("8", "5"),
        ("11", "8"),
        ("5", "5"),
        ("2", "4")
      )
    }
  }

  "part1" should {
    "work for an example" in {
      val input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      new Three().part1(input) shouldEqual 161
    }
  }

  "part2" should {
    "work for an example" in {
      val input =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
      new Three().part2(input) shouldEqual 48
    }
  }
}
