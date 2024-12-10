package com.advent

class Ten {

  def readable9s(
      map: Array[Array[Int]],
      i: Int,
      j: Int,
      level: Int
  ): Set[(Int, Int)] = {
    if (i < 0 || j < 0 || i >= map.length || j >= map(0).length) {
      Set()
    } else {
      val height = map(i)(j)
      if (height != level) {
        Set()
      } else if (height == 9) {
        Set((i, j))
      } else {
        val left = readable9s(map, i, j - 1, level + 1)
        val right = readable9s(map, i, j + 1, level + 1)
        val up = readable9s(map, i - 1, j, level + 1)
        val down = readable9s(map, i + 1, j, level + 1)
        left ++ right ++ up ++ down
      }
    }
  }

  def part1(map: Array[Array[Int]]): Int = {
    var score = 0
    for (i <- map.indices) {
      for (j <- map(0).indices) {
        if (map(i)(j) == 0) {
          val res = readable9s(map, i, j, 0)
          score += res.size
        }
      }
    }
    score
  }

  def part2(map: Array[Array[Int]]): Int = {
    var score = 0
    for (i <- map.indices) {
      for (j <- map(0).indices) {
        if (map(i)(j) == 0) {
          val res = rating(map, i, j, 0)
          score += res
        }
      }
    }
    score
  }

  def rating(
      map: Array[Array[Int]],
      i: Int,
      j: Int,
      level: Int
  ): Int = {
    if (i < 0 || j < 0 || i >= map.length || j >= map(0).length) {
      0
    } else {
      val height = map(i)(j)
      if (height != level) {
        0
      } else if (height == 9) {
        1
      } else {
        val left = rating(map, i, j - 1, level + 1)
        val right = rating(map, i, j + 1, level + 1)
        val up = rating(map, i - 1, j, level + 1)
        val down = rating(map, i + 1, j, level + 1)
        left + right + up + down
      }
    }
  }
}
