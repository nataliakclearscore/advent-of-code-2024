package com.advent

case class Robot(p: (Int, Int), v: (Int, Int))

class Fourteen {

  def positionAfter(robot: Robot, max: (Int, Int), seconds: Int): (Int, Int) = {
    val (px, py) = robot.p
    val (vx, vy) = robot.v
    val (maxX, maxY) = max
    val newX = (px + vx * seconds) % maxX
    val newY = (py + vy * seconds) % maxY
    (if (newX < 0) newX + maxX else newX, if (newY < 0) newY + maxY else newY)
  }

  def part1(robots: List[Robot], max: (Int, Int), seconds: Int): Int = {
    val (midX, midY) = (max._1 / 2, max._2 / 2)
    val q1 = (x: Int, y: Int) => x < midX && y < midY
    val q2 = (x: Int, y: Int) => x > midX && y < midY
    val q3 = (x: Int, y: Int) => x < midX && y > midY
    val q4 = (x: Int, y: Int) => x > midX && y > midY
    val positions =
      robots.map(positionAfter(_, max, seconds))
    val sums = positions.groupBy { case (x, y) =>
      if (q1(x, y)) "q1"
      else if (q2(x, y)) "q2"
      else if (q3(x, y)) "q3"
      else if (q4(x, y)) "q4"
      else "middle"
    }
    sums.getOrElse("q1", List.empty).size *
      sums.getOrElse("q2", List.empty).size *
      sums.getOrElse("q3", List.empty).size *
      sums.getOrElse("q4", List.empty).size
  }

  def part2print(robots: List[Robot], max: (Int, Int)): Unit = {
    val map: Array[Array[Int]] = Array.fill(max._2)(Array.fill(max._1)(0))
    robots.foreach { robot =>
      val (px, py) = robot.p
      map(py)(px) = map(py)(px) + 1
    }
    map.foreach { row =>
      println(row.map(n => if (n == 0) '.' else n).mkString(""))
    }
  }

  // using deviation from the center of the grid
  def remindsChristmasTree(robots: List[Robot], max: (Int, Int)): Boolean = {
    val (midX, midY) = (max._1 / 2, max._2 / 2)
    robots
      .map(p => {
        val (x, y) = p.p
        (x - midX, y - midY)
      })
      .map(p => scala.math.sqrt(p._1 * p._1) + scala.math.sqrt(p._2 * p._2))
      .sum < 16000
  }

  def part2(robots: List[Robot], max: (Int, Int)): Option[Int] =
    (1 to 90280)
      .find(i => {
        val movedRobots = robots.map(robot => {
          val newP = positionAfter(robot, max, i)
          Robot(newP, robot.v)
        })
        remindsChristmasTree(movedRobots, max)
      })
}
