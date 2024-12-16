package com.advent

import com.advent.Direction.{East, South}

import scala.collection.*
import scala.collection.mutable.PriorityQueue

case class Location(i: Int, j: Int)
enum Direction:
  case North, South, East, West
case class Position(location: Location, direction: Direction)

class Sixteen {

  val MoveEast = (loc: Location) => Location(loc.i, loc.j + 1)
  val MoveWest = (loc: Location) => Location(loc.i, loc.j - 1)
  val MoveSouth = (loc: Location) => Location(loc.i + 1, loc.j)
  val MoveNorth = (loc: Location) => Location(loc.i - 1, loc.j)

  def rotationCost(dir: Direction, newDir: Direction): Int = dir match
    case Direction.North => newDir match {
      case Direction.North => 0
      case Direction.South => 2000
      case Direction.East => 1000
      case Direction.West => 1000
    }
    case Direction.South => newDir match {
      case Direction.North => 2000
      case Direction.South => 0
      case Direction.East => 1000
      case Direction.West => 1000
    }
    case Direction.East => newDir match {
      case Direction.North => 1000
      case Direction.South => 1000
      case Direction.East => 0
      case Direction.West => 2000
    }
    case Direction.West => newDir match {
      case Direction.North => 1000
      case Direction.South => 1000
      case Direction.East => 2000
      case Direction.West => 0
    }

  def dijkstras(grid: Array[Array[Char]], start: Location, end: Location): Int = {
    val visited = mutable.Set[Location]()
    val bestScores = mutable.Map[Location, Int]()
    val queue: PriorityQueue[(Position, Int)] = PriorityQueue.empty[(Position, Int)](Ordering.by((_: (Position, Int))._2).reverse)
    var stop = false

    queue.enqueue((Position(start, East), 0))
    while (!stop && queue.nonEmpty) {
      val (position, score) = queue.dequeue()
      if(!visited.contains(position.location)) {
        visited.add(position.location)
        bestScores.put(position.location, score)

        if (position.location == end) {
          stop = true
        } else {

          // move north
          val newLocN = MoveNorth(position.location)
          if (grid(newLocN.i)(newLocN.j) != '#' && !visited.contains(newLocN)) {
            val scoreN = score + 1 + rotationCost(position.direction, Direction.North)
            if (!bestScores.contains(newLocN) || scoreN < bestScores(newLocN)) {
              queue.enqueue((Position(newLocN, Direction.North), scoreN))
            }
          }

          // move south
          val newLocS = MoveSouth(position.location)
          if (grid(newLocS.i)(newLocS.j) != '#' && !visited.contains(newLocS)) {
            val scoreS = score + 1 + rotationCost(position.direction, Direction.South)
            if (!bestScores.contains(newLocS) || scoreS < bestScores(newLocS)) {
              queue.enqueue((Position(newLocS, Direction.South), scoreS))
            }
          }

          // move east
          val newLocE = MoveEast(position.location)
          if (grid(newLocE.i)(newLocE.j) != '#' && !visited.contains(newLocE)) {
            val scoreE = score + 1 + rotationCost(position.direction, Direction.East)
            if (!bestScores.contains(newLocE) || scoreE < bestScores(newLocE)) {
              queue.enqueue((Position(newLocE, Direction.East), scoreE))
            }
          }

          // move west
          val newLocW = MoveWest(position.location)
          if (grid(newLocW.i)(newLocW.j) != '#' && !visited.contains(newLocW)) {
            val scoreW = score + 1 + rotationCost(position.direction, Direction.West)
            if (!bestScores.contains(newLocW) || scoreW < bestScores(newLocW)) {
              queue.enqueue((Position(newLocW, Direction.West), scoreW))
            }
          }
        }
      }
    }

    bestScores(end)
  }

  def part1(grid: Array[Array[Char]]): Int = {
    val start: Location = find(grid, 'S')
    val end: Location = find(grid, 'E')
    dijkstras(grid, start, end)
  }

  private def find(grid: Array[Array[Char]], c: Char) = {
    Location(
      grid.indexWhere(_.contains(c)),
      grid.find(_.contains(c)).get.indexOf(c)
    )
  }
}
