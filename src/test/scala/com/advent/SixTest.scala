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

  "isLooped" should {
    "work for an example" in {
      val input =
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#.#^.....
          |........#.
          |#.........
          |......#...""".stripMargin
      val arr: Array[Array[String]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString))
      new Six().isLooped(arr, 6, 4, 'N') shouldBe true
    }
  }

  "part2" should {
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
      val (res, obstacles) = new Six().part2(arr)

      res.foreach(row => println(row.mkString))
      obstacles.foreach((i, j) => res(i)(j) = 'O')
      println("Obstacles")
      res.foreach(row => println(row.mkString))
      println(obstacles.mkString)
      obstacles.size shouldBe 6
    }
  }
}
