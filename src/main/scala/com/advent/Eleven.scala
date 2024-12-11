package com.advent

class Eleven {

  def blink1(input: List[Long]): List[Long] = {
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

  def blinkN1(input: List[Long], times: Int): Int = {
    val res = (1 to times).foldLeft(input) { (acc, _) =>
      blink1(acc)
    }
    res.size
  }

  def part1(input: String): Int = {
    val inputList = input.split(" ").map(_.toLong).toList
    blinkN1(inputList, 25)
  }

  def blinkN2(
      input: List[Long],
      times: Int
  ): Long = {

    val cache = collection.mutable.Map.empty[(Long, Int), Long] // (input, times) -> result

    def blinkN2(input: Long, times: Int): Long = {
      if (times == 0) {
        1L
      } else if (cache.contains((input, times))) {
        cache((input, times))
      } else {
        val res = input match {
          case 0 =>
            blinkN2(1, times - 1)
          case x if x.toString.length % 2 == 0 =>
            val split = x.toString.splitAt(x.toString.length / 2)
            blinkN2(split._1.toInt, times - 1) + blinkN2(
              split._2.toInt,
              times - 1
            )
          case x =>
            blinkN2(2024 * x, times - 1)
        }
        if (times > 2) cache((input, times)) = res
        res
      }
    }

    var count: Long = 0L
    for (num <- input) {
      count += blinkN2(num, times)
    }
    count
  }

  def part2(input: String): Long = {
    val inputList = input.split(" ").map(_.toLong).toList
    blinkN2(inputList, 75)
  }
}
