package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TenTest extends AnyWordSpec with should.Matchers {

  "scoreForLevel" should {
    "work for an example 1" in {
      val input =
        """9990999
          |9991999
          |9992999
          |6543456
          |7000007
          |8000008
          |9000009""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().readable9s(arr, 0, 3, 0).size shouldEqual 2
    }

    "work for an example 2" in {
      val input =
        """0090009
          |1111198
          |2222227
          |6543456
          |7650987
          |8760000
          |9870000""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().readable9s(arr, 0, 3, 0).size shouldEqual 4
    }

    "work for an example 3" in {
      val input =
        """1022922
          |2222822
          |3222722
          |4567654
          |2228223
          |2229222
          |2222201""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().readable9s(arr, 0, 1, 0).size shouldEqual 1
      new Ten().readable9s(arr, 6, 5, 0).size shouldEqual 2
    }
  }

  "part1" should {
    "work for an example" in {
      val input =
        """89010123
          |78121874
          |87430965
          |96549874
          |45678903
          |32019012
          |01329801
          |10456732""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().part1(arr) shouldEqual 36
    }
  }

  "rating" should {
    "work for an example 1" in {
      val input =
        """9999900
          |9943210
          |9959929
          |9965439
          |9979949
          |0087650
          |0090000""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().rating(arr, 0, 5, 0) shouldEqual 3
    }
  }

  "part2" should {
    "work for an example 1" in {
      val input =
        """012345
          |123456
          |234567
          |345678
          |406789
          |567890""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().part2(arr) shouldEqual 227
    }

    "work for an example 2" in {
      val input =
        """89010123
          |78121874
          |87430965
          |96549874
          |45678903
          |32019012
          |01329801
          |10456732""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().part2(arr) shouldEqual 81
    }

    "work for an example 3" in {
      val input =
        """0090009
          |7771798
          |0002007
          |6543456
          |7650987
          |8760000
          |9870000""".stripMargin
      val arr: Array[Array[Int]] =
        input.split("\n").map(_.trim.toCharArray.map(_.toString.toInt))
      new Ten().part2(arr) shouldEqual 13
    }
  }
}
