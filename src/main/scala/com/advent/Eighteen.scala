package com.advent

import scala.collection.*
import scala.collection.mutable.PriorityQueue

class Eighteen {

  def validIJ(arr: Array[Array[Char]], i: Int, j: Int): Boolean = {
    i >= 0 & j >= 0 & i < arr.length & j < arr(0).length
  }

  val MoveEast = (loc: Location) => Location(loc.i, loc.j + 1)
  val MoveWest = (loc: Location) => Location(loc.i, loc.j - 1)
  val MoveSouth = (loc: Location) => Location(loc.i + 1, loc.j)
  val MoveNorth = (loc: Location) => Location(loc.i - 1, loc.j)

  def dijkstras(
      grid: Array[Array[Char]],
      start: Location,
      end: Location
  ): Int = {
    val visited = mutable.Set[Location]()
    val bestScores = mutable.Map[Location, Int]()
    val queue: PriorityQueue[(Location, Int)] =
      PriorityQueue.empty[(Location, Int)](
        Ordering.by((_: (Location, Int))._2).reverse
      )
    var stop = false

    queue.enqueue((start, 0))
    while (!stop && queue.nonEmpty) {
      val (location, score) = queue.dequeue()
      if (!visited.contains(location)) {
        visited.add(location)
        bestScores.put(location, score)

        if (location == end) {
          stop = true
        } else {

          // move north
          val newLocN = MoveNorth(location)
          if (
            validIJ(grid, newLocN.i, newLocN.j) && grid(newLocN.i)(
              newLocN.j
            ) != '#' && !visited.contains(newLocN)
          ) {
            val scoreN = score + 1
            if (scoreN < bestScores.getOrElse(newLocN, Int.MaxValue)) {
              queue.enqueue((newLocN, scoreN))
            }
          }

          // move south
          val newLocS = MoveSouth(location)
          if (
            validIJ(grid, newLocS.i, newLocS.j) && grid(newLocS.i)(
              newLocS.j
            ) != '#' && !visited.contains(newLocS)
          ) {
            val scoreS = score + 1
            if (scoreS < bestScores.getOrElse(newLocS, Int.MaxValue)) {
              queue.enqueue((newLocS, scoreS))
            }
          }

          // move east
          val newLocE = MoveEast(location)
          if (
            validIJ(grid, newLocE.i, newLocE.j) && grid(newLocE.i)(
              newLocE.j
            ) != '#' && !visited.contains(newLocE)
          ) {
            val scoreE = score + 1
            if (scoreE < bestScores.getOrElse(newLocE, Int.MaxValue)) {
              queue.enqueue((newLocE, scoreE))
            }
          }

          // move west
          val newLocW = MoveWest(location)
          if (
            validIJ(grid, newLocW.i, newLocW.j) && grid(newLocW.i)(
              newLocW.j
            ) != '#' && !visited.contains(newLocW)
          ) {
            val scoreW = score + 1
            if (scoreW < bestScores.getOrElse(newLocW, Int.MaxValue)) {
              queue.enqueue((newLocW, scoreW))
            }
          }
        }
      }
    }

    bestScores(end)
  }

  def part1(list: List[(Int, Int)], gridSize: Int, maxElements: Int): Int = {
    val grid = Array.fill(gridSize)(Array.fill(gridSize)('.'))
    for (n <- 0 until maxElements) {
      val (i, j) = list(n)
      grid(i)(j) = '#'
    }
    dijkstras(grid, Location(0, 0), Location(gridSize - 1, gridSize - 1))
  }

  def dijkstrasPart2(
      grid: Array[Array[Char]],
      start: Location,
      startScore: Int,
      visited: mutable.Set[Location],
      bestScores: mutable.Map[Location, Int]
  ): Unit = {
    val queue: PriorityQueue[(Location, Int)] =
      PriorityQueue.empty[(Location, Int)](
        Ordering.by((_: (Location, Int))._2).reverse
      )

    queue.enqueue((start, startScore))
    while (queue.nonEmpty) {
      val (location, score) = queue.dequeue()
      if (!visited.contains(location)) {
        visited.add(location)
        bestScores.put(location, score)

        // move north
        val newLocN = MoveNorth(location)
        if (
          validIJ(grid, newLocN.i, newLocN.j) && grid(newLocN.i)(
            newLocN.j
          ) != '#' && !visited.contains(newLocN)
        ) {
          val scoreN = score + 1
          if (scoreN < bestScores.getOrElse(newLocN, Int.MaxValue)) {
            queue.enqueue((newLocN, scoreN))
          }
        }

        // move south
        val newLocS = MoveSouth(location)
        if (
          validIJ(grid, newLocS.i, newLocS.j) && grid(newLocS.i)(
            newLocS.j
          ) != '#' && !visited.contains(newLocS)
        ) {
          val scoreS = score + 1
          if (scoreS < bestScores.getOrElse(newLocS, Int.MaxValue)) {
            queue.enqueue((newLocS, scoreS))
          }
        }

        // move east
        val newLocE = MoveEast(location)
        if (
          validIJ(grid, newLocE.i, newLocE.j) && grid(newLocE.i)(
            newLocE.j
          ) != '#' && !visited.contains(newLocE)
        ) {
          val scoreE = score + 1
          if (scoreE < bestScores.getOrElse(newLocE, Int.MaxValue)) {
            queue.enqueue((newLocE, scoreE))
          }
        }

        // move west
        val newLocW = MoveWest(location)
        if (
          validIJ(grid, newLocW.i, newLocW.j) && grid(newLocW.i)(
            newLocW.j
          ) != '#' && !visited.contains(newLocW)
        ) {
          val scoreW = score + 1
          if (scoreW < bestScores.getOrElse(newLocW, Int.MaxValue)) {
            queue.enqueue((newLocW, scoreW))

          }
        }
      }
    }
  }

  def part2(list: List[(Int, Int)], gridSize: Int): (Int, Int) = {
    val grid = Array.fill(gridSize)(Array.fill(gridSize)('.'))
    for (n <- list.indices) {
      val (i, j) = list(n)
      grid(i)(j) = '#'
    }
    val end = Location(gridSize - 1, gridSize - 1)

    val visited = mutable.Set[Location]()
    val bestScores = mutable.Map[Location, Int]()
    val queue: PriorityQueue[(Location, Int)] =
      PriorityQueue.empty[(Location, Int)](
        Ordering.by((_: (Location, Int))._2).reverse
      )
    dijkstrasPart2(
      grid,
      Location(0, 0),
      0,
      visited,
      bestScores
    )
    var n = list.length - 1
    while (n >= 0 && !bestScores.contains(end)) {
      val removed = Location(list(n)._1, list(n)._2)
      grid(removed.i)(removed.j) = '.'

      val neighboursScores = List(
        MoveNorth(removed),
        MoveSouth(removed),
        MoveEast(removed),
        MoveWest(removed)
      ).flatMap(loc => bestScores.get(loc).map((loc, _)))

      if (neighboursScores.nonEmpty) {
        val (loc, score) = neighboursScores.minBy(_._2)
        dijkstrasPart2(
          grid,
          removed,
          score + 1,
          visited,
          bestScores
        )
      }
      n = n - 1
    }

    list(n + 1)
  }

}
