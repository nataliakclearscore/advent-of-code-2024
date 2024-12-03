package com.advent

import scala.math.abs

class Two {

  private def parseInput(input: String): List[List[Int]] =
    input.split("\n").toList.map(_.trim.split(" ").map(_.toInt).toList)

  private def findDiffs(numbers: List[Int]): List[Int] =
    numbers.dropRight(1).zip(numbers.tail).map((a, b) => a - b)

  private def part1Line(numbers: List[Int]): Boolean = {
    val diffs = findDiffs(numbers)
    diffs.forall(abs(_) <= 3) && (diffs.forall(_ < 0) || diffs.forall(_ > 0))
  }

  def part1Lists(input: String): List[Boolean] =
    parseInput(input).map(part1Line)

  def part1(input: String): Int = part1Lists(input).count(identity)

  private def removeInd(numbers: List[Int], ind: Int): List[Int] =
    numbers.take(ind) ++ numbers.drop(ind + 1)

  private def part2Line(numbers: List[Int]): Boolean =
    if (part1Line(numbers)) true
    else {
      val diffs = findDiffs(numbers)
      // find index of the first diff that makes report not safe
      val ind = diffs.zipWithIndex
        .find { case (diff, _) =>
          abs(diff) == 0 || abs(diff) > 3 || diff * diffs.head < 0
        }
        .map(_._2)
        .get
      // if removing the element around the index makes the report safe, return true
      part1Line(removeInd(numbers, ind - 1))
      || part1Line (removeInd(numbers, ind))
      || part1Line (removeInd(numbers, ind + 1))
    }

  def part2Lists(input: String): List[Boolean] =
    parseInput(input).map(part2Line)

  def part2(input: String): Int = part2Lists(input).count(identity)
}
