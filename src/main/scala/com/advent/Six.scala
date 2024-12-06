package com.advent

class Six {

  def validIJ(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    i >= 0 & j >= 0 & i < arr.length & j < arr(0).length
  }

  def findStartingIJ(arr: Array[Array[Char]]): (Int, Int) = {
    var strtingI = -1
    var startingJ = -1
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (arr(i)(j) == '^') {
          strtingI = i
          startingJ = j
        }
      }
    }
    (strtingI, startingJ)
  }

  def move(
      arr: Array[Array[Char]],
      i: Int,
      j: Int,
      direction: Char
  ): (Int, Int, Char) = {
    val (newI, newJ) = direction match {
      case 'N' => (i - 1, j)
      case 'E' => (i, j + 1)
      case 'S' => (i + 1, j)
      case 'W' => (i, j - 1)
    }
    if (validIJ(arr, newI, newJ) && arr(newI)(newJ) == '#') {
      val newDirection = directions((directions.indexOf(direction) + 1) % 4)
      (i, j, newDirection)
    } else {
      (newI, newJ, direction)
    }
  }

  val directions: List[Char] = List('N', 'E', 'S', 'W')

  def part1(arr: Array[Array[Char]]): Array[Array[Char]] = {
    var (i, j) = findStartingIJ(arr)
    var direction = 'N'
    while (validIJ(arr, i, j)) {
      arr(i)(j) = 'X'
      val (newI, newJ, newDirection) = move(arr, i, j, direction)
      i = newI
      j = newJ
      direction = newDirection
    }
    arr
  }
}
