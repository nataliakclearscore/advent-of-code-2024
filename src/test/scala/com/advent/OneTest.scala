package com.advent

import org.scalatest._
import flatspec._
import matchers._

class OneTest extends AnyFlatSpec with should.Matchers {
  "One" should "work for an example" in {
    val input = """3   4
              4   3
              2   5
              1   3
              3   9
              3   3""".stripMargin
    new One().process(input) shouldEqual 11
  }
}
