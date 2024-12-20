package com.advent

import scala.collection.*
import scala.collection.mutable.PriorityQueue

class Twenty {

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
  ): mutable.Map[Location, Int] = {
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

    bestScores
  }

  private def find(grid: Array[Array[Char]], c: Char) = {
    Location(
      grid.indexWhere(_.contains(c)),
      grid.find(_.contains(c)).get.indexOf(c)
    )
  }

  def findSavings(
      loc1: Location,
      loc2: Location,
      bestScores: mutable.Map[Location, Int],
      grid: Array[Array[Char]]
  ): Option[Int] = {
    if (
      validIJ(grid, loc1.i, loc1.j)
      && validIJ(grid, loc2.i, loc2.j)
      && grid(loc1.i)(loc1.j) == '.'
      && grid(loc2.i)(loc2.j) == '.'
      && bestScores.contains(loc1)
      && bestScores.contains(loc2)
    ) {
      val bestScoresEast = bestScores.get(loc1)
      val bestScoresWest = bestScores.get(loc2)
      val saved: Int =
        scala.math.abs(bestScoresEast.get - bestScoresWest.get) - 2
      Some(saved)
    } else {
      None
    }
  }

  def findSavingsPart1(grid: Array[Array[Char]]): mutable.Map[Int, Int] = {
    val start: Location = find(grid, 'S')
    val end: Location = find(grid, 'E')
    grid(start.i)(start.j) = '.'
    grid(end.i)(end.j) = '.'
    val bestScores = dijkstras(grid, start, end)

    val canRemove = mutable.Set[Location]()
    for (i <- 1 to grid.length - 1) {
      for (j <- 1 to grid(0).length - 1) {
        if (grid(i)(j) == '#') {
          canRemove.add(Location(i, j))
        }
      }
    }

    val savings = mutable.Map[Int, Int]()

    for (remove <- canRemove) {
      // try removing horizontal
      val east = MoveEast(remove)
      val west = MoveWest(remove)
      val savedHorizontal = findSavings(east, west, bestScores, grid)
      savedHorizontal.foreach(saved =>
        savings.put(saved, savings.getOrElse(saved, 0) + 1)
      )

      val north = MoveNorth(remove)
      val south = MoveSouth(remove)
      val savedVertical = findSavings(north, south, bestScores, grid)
      savedVertical.foreach(saved =>
        savings.put(saved, savings.getOrElse(saved, 0) + 1)
      )
    }
    savings
  }

  def part1(grid: Array[Array[Char]]): Int = {
    val savings = findSavingsPart1(grid)
    savings.filter(_._1 >= 100).values.sum
  }

  def findSavingsPart2(grid: Array[Array[Char]], minSaved: Int = 100): mutable.Map[Int, Int] = {
    val start: Location = find(grid, 'S')
    val end: Location = find(grid, 'E')
    grid(start.i)(start.j) = '.'
    grid(end.i)(end.j) = '.'
    val bestScoresList = dijkstras(grid, start, end).toList
    println(s"bestScoresList size: ${bestScoresList.size}")

    val savings = mutable.Map[Int, Int]()
    for (i <- bestScoresList.indices) {
      println(s"i: $i")
      for (j <- i + 1 until bestScoresList.length) {
        val bs1 = bestScoresList(i)._1
        val bs2 = bestScoresList(j)._1
        val minScoreDiff = math.abs(bs1.i - bs2.i) + math.abs(bs1.j - bs2.j)
        if (minScoreDiff <= 20) {
          val scoreDiff = math.abs(bestScoresList(i)._2 - bestScoresList(j)._2)
          val saved = scoreDiff - minScoreDiff
          if (minScoreDiff < scoreDiff && saved >= minSaved) {
            savings.put(saved, savings.getOrElse(saved, 0) + 1)
          }
        }
      }
    }
    savings
  }

  def part2(grid: Array[Array[Char]]): Int = {
    val savings = findSavingsPart2(grid)
    savings.filter(_._1 >= 100).values.sum
  }
}
