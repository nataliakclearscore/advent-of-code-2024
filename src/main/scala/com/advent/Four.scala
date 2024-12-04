package com.advent

class Four {

  def validI(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    i >= 0 & j >= 0 & i < arr.length & j < arr(0).length
  }

  def find(
      arr: Array[Array[Char]],
      i1: Int,
      j1: Int,
      f: (Int, Int) => (Int, Int)
  ): Boolean = {
    val (i2, j2) = f(i1, j1)
    val (i3, j3) = f(i2, j2)
    val (i4, j4) = f(i3, j3)
    validI(arr, i1, j1)
    && validI (arr, i2, j2)
    && validI (arr, i3, j3)
    && validI (arr, i4, j4)
    && arr (i1)(j1) == 'X'
    && arr (i2)(j2) == 'M'
    && arr (i3)(j3) == 'A'
    && arr (i4)(j4) == 'S'
  }

  val Horizontal1 = (i: Int, j: Int) => (i, j + 1)
  val Horizontal2 = (i: Int, j: Int) => (i, j - 1)
  val Vertical1 = (i: Int, j: Int) => (i + 1, j)
  val Vertical2 = (i: Int, j: Int) => (i - 1, j)
  val Diagonal1 = (i: Int, j: Int) => (i + 1, j + 1)
  val Diagonal2 = (i: Int, j: Int) => (i - 1, j - 1)
  val Diagonal3 = (i: Int, j: Int) => (i + 1, j - 1)
  val Diagonal4 = (i: Int, j: Int) => (i - 1, j + 1)

  def findAll(arr: Array[Array[Char]], i: Int, j: Int): Int = {
    val results = List(
      find(arr, i, j, Horizontal1),
      find(arr, i, j, Horizontal2),
      find(arr, i, j, Vertical1),
      find(arr, i, j, Vertical2),
      find(arr, i, j, Diagonal1),
      find(arr, i, j, Diagonal2),
      find(arr, i, j, Diagonal3),
      find(arr, i, j, Diagonal4)
    )
    results.count(identity)
  }

  def part1(input: String): Int = {
    val arr: Array[Array[Char]] = input.split("\n").map(_.trim.toCharArray)
    var count: Int = 0
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        count += findAll(arr, i, j)
      }
    }
    count
  }

  //M.S
  //.A.
  //M.S
  def crossMatch1(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    validI(arr, i, j)
    && arr (i)(j) == 'M'
    && validI (arr, i, j + 2)
    && arr (i)(j + 2) == 'S'
    && validI (arr, i + 1, j + 1)
    && arr (i + 1)(j + 1) == 'A'
    && validI (arr, i + 2, j)
    && arr (i + 2)(j) == 'M'
    && validI (arr, i + 2, j + 2)
    && arr (i + 2)(j + 2) == 'S'
  }

  //M.M
  //.A.
  //S.S
  def crossMatch2(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    validI(arr, i, j)
    && arr (i)(j) == 'M'
    && validI (arr, i, j + 2)
    && arr (i)(j + 2) == 'M'
    && validI (arr, i + 1, j + 1)
    && arr (i + 1)(j + 1) == 'A'
    && validI (arr, i + 2, j)
    && arr (i + 2)(j) == 'S'
    && validI (arr, i + 2, j + 2)
    && arr (i + 2)(j + 2) == 'S'
  }

  //S.S
  //.A.
  //M.M
  def crossMatch3(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    validI(arr, i, j)
    && arr (i)(j) == 'S'
    && validI (arr, i, j + 2)
    && arr (i)(j + 2) == 'S'
    && validI (arr, i + 1, j + 1)
    && arr (i + 1)(j + 1) == 'A'
    && validI (arr, i + 2, j)
    && arr (i + 2)(j) == 'M'
    && validI (arr, i + 2, j + 2)
    && arr (i + 2)(j + 2) == 'M'
  }

  //S.M
  //.A.
  //S.M
  def crossMatch4(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    validI(arr, i, j)
    && arr (i)(j) == 'S'
    && validI (arr, i, j + 2)
    && arr (i)(j + 2) == 'M'
    && validI (arr, i + 1, j + 1)
    && arr (i + 1)(j + 1) == 'A'
    && validI (arr, i + 2, j)
    && arr (i + 2)(j) == 'S'
    && validI (arr, i + 2, j + 2)
    && arr (i + 2)(j + 2) == 'M'
  }

  def crossMatch(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    crossMatch1(arr, i, j)
    || crossMatch2 (arr, i, j)
    || crossMatch3 (arr, i, j)
    || crossMatch4 (arr, i, j)
  }

  def part2(input: String): Int = {
    val arr: Array[Array[Char]] = input.split("\n").map(_.trim.toCharArray)
    var count: Int = 0
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (crossMatch(arr, i, j)) {
          count += 1
        }
      }
    }
    count
  }
}
