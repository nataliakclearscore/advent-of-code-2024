package com.advent

import scala.math.*

class Seven {

  def part1Line(testValue: Long, nums: List[Long]): Boolean =
    part1Line(testValue, nums, 1, nums.head)

  def part1Line(
      testValue: Long,
      nums: List[Long],
      i: Int,
      res: Long
  ): Boolean = {
    if (i == nums.length && res == testValue) {
      true
    } else if (i >= nums.length || res > testValue) {
      false
    } else {
      part1Line(testValue, nums, i + 1, res * nums(i)) ||
      part1Line(testValue, nums, i + 1, res + nums(i))
    }
  }

  def part2Line(testValue: BigInt, nums: List[BigInt]): Boolean = {
    val ops = List[(BigInt, BigInt) => BigInt](
      _ * _,
      _ + _,
      (a, b) => BigInt(a.toString + b.toString)
    )
    part2Line(ops, testValue, nums, 1, nums.head)
  }

  def part2Line(
      ops: List[(BigInt, BigInt) => BigInt],
      testValue: BigInt,
      nums: List[BigInt],
      i: Int,
      res: BigInt
  ): Boolean = {
    if (i == nums.length && res == testValue) {
      true
    } else if (i >= nums.length || res > testValue) {
      false
    } else {
      ops.exists { op =>
        part2Line(ops, testValue, nums, i + 1, op(res, nums(i)))
      }
    }
  }
}
