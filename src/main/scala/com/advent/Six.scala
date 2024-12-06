package com.advent

class Six {

  def validIJ[T](arr: Array[Array[T]], i: Int, j: Int): Boolean = {
    i >= 0 & j >= 0 & i < arr.length & j < arr(0).length
  }

  def newIJ(i: Int, j: Int, direction: Char) = {
    val (newI, newJ) = direction match {
      case 'N' => (i - 1, j)
      case 'E' => (i, j + 1)
      case 'S' => (i + 1, j)
      case 'W' => (i, j - 1)
    }
    (newI, newJ)
  }

  def findStartingIJ(arr: Array[Array[Char]]): (Int, Int) = {
    var startingI = -1
    var startingJ = -1
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (arr(i)(j) == '^') {
          startingI = i
          startingJ = j
        }
      }
    }
    (startingI, startingJ)
  }

  def move(
      arr: Array[Array[Char]],
      i: Int,
      j: Int,
      direction: Char
  ): (Int, Int, Char) = {
    val (newI: Int, newJ: Int) = newIJ(i, j, direction)
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

  def isLooped(
      arr: Array[Array[String]],
      startingI: Int,
      startingJ: Int,
      startingDirection: Char
  ): Boolean = {
    var (i, j) = (startingI, startingJ)
    var direction = startingDirection
    var looped = false
    while (validIJ(arr, i, j) && !looped) {
      if (arr(i)(j).contains(direction.toString)) { // been here before moving in this direction
        looped = true
      }
      arr(i)(j) = arr(i)(j) + direction.toString
      val (newI: Int, newJ: Int) = newIJ(i, j, direction)
      if (validIJ(arr, newI, newJ) && arr(newI)(newJ) == "#") {
        // rotate
        direction = directions((directions.indexOf(direction) + 1) % 4)
      } else {
        // move forward
        i = newI
        j = newJ
      }
    }
    looped
  }

  def part2(
      original: Array[Array[Char]]
  ): (Array[Array[Char]], List[(Int, Int)]) = {
    val arr = original.map(_.clone)
    val (startingI, startingJ) = findStartingIJ(arr)
    var (i, j) = (startingI, startingJ)
    var direction = 'N'
    var triedObstacles: List[(Int, Int)] = List()
    var loopingObstacles: List[(Int, Int)] = List()
    while (validIJ(arr, i, j)) {
      val (newI: Int, newJ: Int) = newIJ(i, j, direction)
      if (validIJ(arr, newI, newJ) && arr(newI)(newJ) == '#') { // need to rotate
        direction = directions((directions.indexOf(direction) + 1) % 4)
      } else { // can move forward
        if (
          validIJ(arr, newI, newJ)
          && !(newI == startingI && newJ == startingJ)
          && !triedObstacles.contains((newI, newJ))
        ) { // not moving out of bounds so obstacle can be here
          // try placing an obstacle here
          triedObstacles = (newI, newJ) :: triedObstacles
          val tryArr = original.map(_.map(_.toString))
          tryArr(newI)(newJ) = "#"
          if (isLooped(tryArr, i, j, direction)) {
            loopingObstacles = (newI, newJ) :: loopingObstacles
          }
        }
        arr(i)(j) = 'X'
        i = newI
        j = newJ
      }
    }
    (arr, loopingObstacles)
  }
}
