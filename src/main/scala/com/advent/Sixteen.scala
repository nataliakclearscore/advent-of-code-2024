package com.advent

import com.advent.Direction._

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
      if (!visited.contains(position.location)) {
        visited.add(position.location)
        bestScores.put(position.location, score)

        if (position.location == end) {
          stop = true
        } else {

          // move north
          val newLocN = MoveNorth(position.location)
          if (grid(newLocN.i)(newLocN.j) != '#' && !visited.contains(newLocN)) {
            val scoreN = score + 1 + rotationCost(position.direction, Direction.North)
            if (scoreN < bestScores.getOrElse(newLocN, Int.MaxValue)) {
              queue.enqueue((Position(newLocN, Direction.North), scoreN))
            }
          }

          // move south
          val newLocS = MoveSouth(position.location)
          if (grid(newLocS.i)(newLocS.j) != '#' && !visited.contains(newLocS)) {
            val scoreS = score + 1 + rotationCost(position.direction, Direction.South)
            if (scoreS < bestScores.getOrElse(newLocS, Int.MaxValue)) {
              queue.enqueue((Position(newLocS, Direction.South), scoreS))
            }
          }

          // move east
          val newLocE = MoveEast(position.location)
          if (grid(newLocE.i)(newLocE.j) != '#' && !visited.contains(newLocE)) {
            val scoreE = score + 1 + rotationCost(position.direction, Direction.East)
            if (scoreE < bestScores.getOrElse(newLocE, Int.MaxValue)) {
              queue.enqueue((Position(newLocE, Direction.East), scoreE))
            }
          }

          // move west
          val newLocW = MoveWest(position.location)
          if (grid(newLocW.i)(newLocW.j) != '#' && !visited.contains(newLocW)) {
            val scoreW = score + 1 + rotationCost(position.direction, Direction.West)
            if (scoreW < bestScores.getOrElse(newLocW, Int.MaxValue)) {
              queue.enqueue((Position(newLocW, Direction.West), scoreW))
            }
          }
        }
      }
    }

    bestScores(end)
  }

  private def find(grid: Array[Array[Char]], c: Char) = {
    Location(
      grid.indexWhere(_.contains(c)),
      grid.find(_.contains(c)).get.indexOf(c)
    )
  }

  def part1(grid: Array[Array[Char]]): Int = {
    val start: Location = find(grid, 'S')
    val end: Location = find(grid, 'E')
    dijkstras(grid, start, end)
  }

  def findPaths(leads: Map[Location, Set[Location]], cur: Location, path: List[Location]): List[List[Location]] = {
    if(leads.contains(cur)) {
      leads(cur).flatMap(lead => findPaths(leads, lead, lead :: path)).toList
    } else {
      List(path)
    }
  }

  def calculateScore(path: List[Location]): Int = {
    var score = 0
    var curLoc = path.head
    var curDir = East
    for(i <- 1 until path.size) {
      val nextLoc = path(i)
      val nextDir = if(nextLoc.i < curLoc.i) {
        North
      } else if(nextLoc.i > curLoc.i) {
        South
      } else if(nextLoc.j < curLoc.j) {
        West
      } else {
        East
      }
      score += 1 + rotationCost(curDir, nextDir)
      curLoc = nextLoc
      curDir = nextDir
    }
    score
  }

  def part2(grid: Array[Array[Char]]): Int = {
    val start: Location = find(grid, 'S')
    val end: Location = find(grid, 'E')
    val (minScore, leads) = dijkstrasWithTracking(grid, start, end) // use leads to find all paths
    val paths = findPaths(leads, end, List(end)) // because of how rotation cost is calculated, paths can contain some extra paths that don't have the minimum score
    paths.map(path => (calculateScore(path), path)).filter(_._1 == minScore).flatMap(_._2).toSet.size
  }

  def dijkstrasWithTracking(grid: Array[Array[Char]], start: Location, end: Location): (Int, Map[Location, Set[Location]]) = {
    val visited = mutable.Set[Location]()
    val bestScores = mutable.Map[Location, (Int, Direction)]()
    val queue: PriorityQueue[(Position, Option[Location], Int)] = PriorityQueue.empty[(Position, Option[Location], Int)](Ordering.by((_: (Position, Option[Location], Int))._3).reverse)
    val leads = mutable.Map[Location, Set[Location]]()

    queue.enqueue((Position(start, East), None, 0))
    while (queue.nonEmpty) {
      val (position, prevLoc, score) = queue.dequeue()
      if (!visited.contains(position.location)) {
        visited.add(position.location)
        bestScores.put(position.location, (score, position.direction))
        if(prevLoc.isDefined) {
          leads.put(position.location, Set(prevLoc.get))
        }

        // move north
        val newLocN = MoveNorth(position.location)
        if (grid(newLocN.i)(newLocN.j) != '#') {
          val scoreN = score + 1 + rotationCost(position.direction, North)
          val bestOpt: Option[(Int, Direction)] = bestScores.get(newLocN)
          val bestScore = bestOpt.map(_._1).getOrElse(Int.MaxValue)
          val bestDir = bestOpt.map(_._2).getOrElse(North)
          //println(s"newLocN: $newLocN, bestScores: ${bestScores.getOrElse(newLocN, Int.MaxValue)} scoreN: $scoreN")
          if (scoreN < bestScore && !visited.contains(newLocN)) {
            queue.enqueue((Position(newLocN, North), Some(position.location), scoreN))
          } else if (scoreN - rotationCost(North, bestDir) == bestScore) { // can look different ways!!
            leads.put(newLocN, leads.getOrElse(newLocN, Set.empty) + position.location)
          }
        }

        // move south
        val newLocS = MoveSouth(position.location)
        if (grid(newLocS.i)(newLocS.j) != '#') {
          val scoreS = score + 1 + rotationCost(position.direction, South)
          val bestOpt: Option[(Int, Direction)] = bestScores.get(newLocS)
          val bestScore = bestOpt.map(_._1).getOrElse(Int.MaxValue)
          val bestDir = bestOpt.map(_._2).getOrElse(South)
          //println(s"newLocS: $newLocS, bestScores: ${bestScores.getOrElse(newLocS, Int.MaxValue)} scoreS: $scoreS")
          if (scoreS < bestScore && !visited.contains(newLocS)) {
            queue.enqueue((Position(newLocS, South), Some(position.location), scoreS))
          } else if (scoreS - rotationCost(South, bestDir) == bestScore) { // can look different ways!!
            leads.put(newLocS, leads.getOrElse(newLocS, Set.empty) + position.location)
          }
        }

        // move east
        val newLocE = MoveEast(position.location)
        if (grid(newLocE.i)(newLocE.j) != '#') {
          val scoreE = score + 1 + rotationCost(position.direction, East)
          val bestOpt: Option[(Int, Direction)] = bestScores.get(newLocE)
          val bestScore = bestOpt.map(_._1).getOrElse(Int.MaxValue)
          val bestDir = bestOpt.map(_._2).getOrElse(East)
          //println(s"newLocE: $newLocE, bestScores: ${bestScores.getOrElse(newLocE, Int.MaxValue)} scoreE: $scoreE")
          if (scoreE < bestScore && !visited.contains(newLocE)) {
            queue.enqueue((Position(newLocE, East), Some(position.location), scoreE))
          } else if (scoreE - rotationCost(East, bestDir) == bestScore) { // can look different ways!!
            leads.put(newLocE, leads.getOrElse(newLocE, Set.empty) + position.location)
          }
        }

        // move west
        val newLocW = MoveWest(position.location)
        if (grid(newLocW.i)(newLocW.j) != '#') {
          val scoreW = score + 1 + rotationCost(position.direction, West)
          val bestOpt: Option[(Int, Direction)] = bestScores.get(newLocW)
          val bestScore = bestOpt.map(_._1).getOrElse(Int.MaxValue)
          val bestDir = bestOpt.map(_._2).getOrElse(West)
          //println(s"newLocW: $newLocW, bestScores: ${bestScores.getOrElse(newLocW, Int.MaxValue)} scoreW: $scoreW")
          if (scoreW < bestScore && !visited.contains(newLocW)) {
            queue.enqueue((Position(newLocW, West), Some(position.location), scoreW))
          } else if (scoreW - rotationCost(West, bestDir) == bestScore) { // can look different ways!!
            leads.put(newLocW, leads.getOrElse(newLocW, Set.empty) + position.location)
          }
        }
      }
    }
    (bestScores(end)._1, leads.toMap)
  }
}
