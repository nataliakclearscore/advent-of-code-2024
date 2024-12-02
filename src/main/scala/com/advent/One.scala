package com.advent

import scala.math.abs


class One {

  private def parseLists(input: String) = {
    val lines = input.split("\n").toList.map(_.trim.split("   ").map(_.toInt))
    val list1 = lines.map(_(0))
    val list2 = lines.map(_(1))
    (list1, list2)
  }

  def part1(input: String): Int = {
    val (list1: List[Int], list2: List[Int]) = parseLists(input)
    list1.sorted.zip(list2.sorted).map { case (a, b) => abs(a - b) }.sum
  }

  def part2(input: String): Int = {
    val (list1: List[Int], list2: List[Int]) = parseLists(input)
    list1.fold(0) { (acc, a) =>
      val count = list2.count(_ == a)
      acc + a * count
    }
  }
}
