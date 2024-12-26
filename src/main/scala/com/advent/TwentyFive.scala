package com.advent

import scala.collection.mutable

case class Lock(hights: List[Int])

case class Key(hights: List[Int])

class TwentyFive {

  private def findHights(input: Array[Array[Char]], start: Int, pin: Int): Int =
    (0 until 7).count(i => input(start + i)(pin) == '#') - 1

  def parse(input: String): (List[Lock], List[Key]) = {
    val lines: Array[Array[Char]] = input.split("\n").map(_.toCharArray)

    val locks = mutable.ListBuffer.empty[Lock]
    val keys = mutable.ListBuffer.empty[Key]

    var i = 0
    while (i < lines.length) {
      if (lines(i) sameElements "#####".toCharArray) { // lock
        locks += Lock(
          List(
            findHights(lines, i, 0),
            findHights(lines, i, 1),
            findHights(lines, i, 2),
            findHights(lines, i, 3),
            findHights(lines, i, 4)
          )
        )
      } else {
        keys += Key(
          List(
            findHights(lines, i, 0),
            findHights(lines, i, 1),
            findHights(lines, i, 2),
            findHights(lines, i, 3),
            findHights(lines, i, 4)
          )
        )
      }

      i = i + 8
    }

    (locks.toList, keys.toList)
  }

  def fits(lock: Lock, key: Key): Boolean =
    lock.hights.zip(key.hights).forall((h1, h2) => h1 + h2 < 6)

  def part1(locks: List[Lock], keys: List[Key]): Int = {
    locks.map(l => keys.count(k => fits(l, k))).sum
  }
}
