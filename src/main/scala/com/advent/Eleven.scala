package com.advent

class Eleven {

  def blink(input: List[Long]): List[Long] = {
    input.flatMap {
      case 0 =>
        List(1)
      case x if x.toString.length % 2 == 0 =>
        val split = x.toString.splitAt(x.toString.length / 2)
        List(split._1.toInt, split._2.toInt)
      case x =>
        List(2024 * x)
    }
  }

  def blinkN(input: List[Long], times: Int): Int = {
    val res = (1 to times).foldLeft(input) { (acc, _) =>
      blink(acc)
    }
    res.size
  }

  def part1(input: String): Int = {
    val inputList = input.split(" ").map(_.toLong).toList
    blinkN(inputList, 25)
  }
}
