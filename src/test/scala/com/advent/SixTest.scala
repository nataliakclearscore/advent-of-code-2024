package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SixTest extends AnyWordSpec with should.Matchers {
  "part1" should {
    "work for an example" in {
      val input =
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#..^.....
          |........#.
          |#.........
          |......#...""".stripMargin
      val arr: Array[Array[Char]] = input.split("\n").map(_.trim.toCharArray)
      val res = new Six().part1(arr)
      for (i <- res.indices) {
        println(res(i).mkString)
      }
      res.foldLeft(0)((acc, row) => acc + row.count(_ == 'X')) shouldEqual 41
    }
  }
}
