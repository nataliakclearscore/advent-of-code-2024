package com.advent

import scala.math.abs

class One {
  def process(input: String): Int = {
    val lines = input.split("\n").toList.map(_.trim.split("   ").map(_.toInt))
    val list1 = lines.map(_(0))
    val list2 = lines.map(_(1))
    list1.sorted.zip(list2.sorted).map { case (a, b) => abs(a - b) }.sum
  }
}
