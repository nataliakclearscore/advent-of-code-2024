package com.advent

import scala.math.abs

class Two {

  private def parseInput(input: String): List[List[Int]] =
    input.split("\n").toList.map(_.trim.split(" ").map(_.toInt).toList)

  private def part1Line(numbers: List[Int]): Boolean = {
    val diffs = numbers.dropRight(1).zip(numbers.tail).map((a, b) => a - b)
    diffs.forall(abs(_) <= 3) && (diffs.forall(_ < 0) || diffs.forall(_ > 0))
  }

  def part1Lists(input: String): List[Boolean] =
    parseInput(input).map(part1Line)

  def part1(input: String): Int = part1Lists(input).count(identity)
}
