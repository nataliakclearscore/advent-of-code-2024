package com.advent

class TwentyTwo {

  def mix(secret: Long, value: Long): Long = secret ^ value

  // 16777216 = 2^24
  def prune(secret: Long): Long = secret & 0xffffff

  def nextSecret(secret: Long): Long = {
    // 64 = 2^6
    val step1 = prune(mix(secret, secret << 6))
    // 32 = 2^5
    val step2 = prune(mix(step1, step1 >> 5))
    // 2048 = 2^11
    val step3 = prune(mix(step2, step2 << 11))
    step3
  }

  def nextSecretN(secret: Long, n: Int): Long = {
    var result = secret
    for (_ <- 0 until n) {
      result = nextSecret(result)
    }
    result
  }

  def part1(secrets: List[Long]): Long = {
    secrets.map(s => nextSecretN(s, 2000)).sum
  }

  def priceSequence(secret: Long, n: Int): List[Int] = {
    val prices = scala.collection.mutable.ListBuffer.empty[Int]
    var currentSecret = secret
    for (_ <- 0 until n) {
      val price = (currentSecret % 10).toInt
      prices += price
      currentSecret = nextSecret(currentSecret)
    }
    val res = prices.toList
    res
  }

  def changesSequence(prices: List[Int]): List[Int] = {
    prices
      .sliding(2)
      .map { case Seq(a, b) =>
        b - a
      }
      .toList
  }

  def buildSequenceMap(prices: List[Int]): Map[String, Int] = {
    val map = scala.collection.mutable.Map.empty[String, Int]
    prices.sliding(5).foreach { window =>
      val key = changesSequence(window).mkString(",")
      val value = window.last
      if (!map.contains(key)) {
        map(key) = value
      }
    }
    map.toMap
  }

  def bestPrice(secrets: List[Long], n: Int): Int = {
    val maps = secrets.map(priceSequence(_, n + 1)).map(buildSequenceMap)
    val allKeys = maps.flatMap(_.keys.toSet).toSet
    
    allKeys.foldLeft(0) { (bestPrice, key) =>
      val values = maps.map(map => map.getOrElse(key, 0))
      val sum = values.sum
      if (sum > bestPrice) sum else bestPrice
    }
  }

  def part2(secrets: List[Long]): Int = {
    bestPrice(secrets, 2000)
  }
}
