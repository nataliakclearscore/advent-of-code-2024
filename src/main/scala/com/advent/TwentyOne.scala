package com.advent

import scala.collection.*

class TwentyOne {

  def validIJ(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    i >= 0 & j >= 0 & i < arr.length & j < arr(0).length
  }

  val MoveEast = (loc: Location) => Location(loc.i, loc.j + 1)
  val MoveWest = (loc: Location) => Location(loc.i, loc.j - 1)
  val MoveSouth = (loc: Location) => Location(loc.i + 1, loc.j)
  val MoveNorth = (loc: Location) => Location(loc.i - 1, loc.j)

  val numericGrid: Array[Array[Char]] =
    """789
      |456
      |123
      |#0A""".stripMargin.split("\n").map(_.toCharArray)

  val directionalGrid: Array[Array[Char]] =
    """#^A
      |<v>""".stripMargin.split("\n").map(_.toCharArray)

  def moveToChar(move: Location => Location): Char = move match {
    case MoveNorth => '^'
    case MoveSouth => 'v'
    case MoveEast  => '>'
    case MoveWest  => '<'
  }

  def findLocation(grid: Array[Array[Char]], char: Char): Location = {
    for (i <- grid.indices) {
      for (j <- grid(0).indices) {
        if (grid(i)(j) == char) {
          return Location(i, j)
        }
      }
    }
    throw new RuntimeException(s"Could not find $char in grid")
  }

  def pressButton(
      grid: Array[Array[Char]],
      start: Location,
      end: Location
  ): mutable.ListBuffer[Char] = {
    val path = mutable.ListBuffer.empty[Char]
    if (end.i < start.i) {
      for (_ <- start.i until end.i by -1) {
        path += '^'
      }
    } else if (end.i > start.i) {
      for (_ <- start.i until end.i) {
        path += 'v'
      }
    }

    if (end.j > start.j) {
      for (_ <- start.j until end.j) {
        path += '>'
      }
    } else if (end.j < start.j) {
      for (_ <- start.j until end.j by -1) {
        path += '<'
      }
    }
    path
  }

  def findButton(
      grid: Array[Array[Char]],
      start: Char,
      end: Char
  ): mutable.ListBuffer[Char] = {
    val startLoc = findLocation(grid, start)
    val endLoc = findLocation(grid, end)
    pressButton(grid, startLoc, endLoc)
  }

  def pressButtons(
      grid: Array[Array[Char]],
      buttons: List[Char]
  ): List[Char] = {
    ('A' :: buttons)
      .sliding(2)
      .flatMap { case List(start, end) =>
        findButton(grid, start, end).append('A')
      }
      .toList
  }

  def pressButtonsAllLayers(buttons: List[Char]): List[Char] = {
    val directions1 = pressButtons(numericGrid, buttons)
    val directions2 = pressButtons(directionalGrid, directions1)
    val directions3 = pressButtons(directionalGrid, directions2)
    directions3
  }

  def part1(lines: List[String]): Int = {
    lines
      .map(line =>
        pressButtonsAllLayers(line.toCharArray.toList).size * line.dropRight(1).toInt
      )
      .sum
  }
}
