package com.advent

class Eight {

  def findAntennas(arr: Array[Array[Char]]): Map[Char, List[(Int, Int)]] = {
    val antennas = collection.mutable.Map[Char, List[(Int, Int)]]()
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (arr(i)(j) != '.') {
          if (antennas.contains(arr(i)(j))) {
            antennas(arr(i)(j)) = (i, j) :: antennas(arr(i)(j))
          } else {
            antennas(arr(i)(j)) = List((i, j))
          }
        }
      }
    }
    antennas.toMap
  }

  def isInside(i: Int, j: Int, maxI: Int, maxJ: Int): Boolean = {
    i >= 0 && j >= 0 && i < maxI && j < maxJ
  }

  def countAntinodes(
      antennas: List[(Int, Int)],
      maxI: Int,
      maxJ: Int
  ): List[(Int, Int)] = {
    var result: List[(Int, Int)] = List()
    for (i <- antennas.indices) {
      if (i + 1 < antennas.length) {
        for (j <- antennas.indices.drop(i + 1)) {
          val antenna1 = antennas(i)
          val antenna2 = antennas(j)
//          println(s"antennas $antenna1, $antenna2")
          val deltaI = antenna1._1 - antenna2._1
          val deltaJ = antenna1._2 - antenna2._2
          val antinode1 = (antenna1._1 + deltaI, antenna1._2 + deltaJ)
          val antinode2 = (antenna2._1 - deltaI, antenna2._2 - deltaJ)
//          println(s"antinodes $antinode1, $antinode2")
          if (isInside(antinode1._1, antinode1._2, maxI, maxJ)) {
            result = antinode1 :: result
          }
          if (isInside(antinode2._1, antinode2._2, maxI, maxJ)) {
            result = antinode2 :: result
          }
        }
      }
    }
    result
  }

  def part1(arr: Array[Array[Char]]): Int = {
    val antennas: Map[Char, List[(Int, Int)]] = findAntennas(arr)
    val maxI = arr.length
    val maxJ = arr(0).length
    val antinodes = antennas
      .map { case (letter, list) =>
        (letter, countAntinodes(list, maxI, maxJ))
      }
    antinodes.values.flatten.toSet.size
  }
}
