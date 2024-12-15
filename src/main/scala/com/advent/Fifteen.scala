package com.advent

class Fifteen {

  val Horizontal1 = (i: Int, j: Int) => (i, j + 1)
  val Horizontal2 = (i: Int, j: Int) => (i, j - 1)
  val Vertical1 = (i: Int, j: Int) => (i + 1, j)
  val Vertical2 = (i: Int, j: Int) => (i - 1, j)

  def moveFunction(command: Char): (Int, Int) => (Int, Int) =
    command match {
      case '^' => Vertical2
      case 'v' => Vertical1
      case '<' => Horizontal2
      case '>' => Horizontal1
      case _   => throw new IllegalArgumentException(s"Invalid command: $command")
    }

  def swap(
      grid: Array[Array[Char]],
      loc1: (Int, Int),
      loc2: (Int, Int)
  ): Unit = {
    val temp = grid(loc1._1)(loc1._2)
    grid(loc1._1)(loc1._2) = grid(loc2._1)(loc2._2)
    grid(loc2._1)(loc2._2) = temp
  }

  def canShiftBoxes(
      grid: Array[Array[Char]],
      box: (Int, Int),
      moveF: (Int, Int) => (Int, Int)
  ): Option[(Int, Int)] = {
    val (boxI, boxJ) = box
    val (boxNewI, boxNewJ) = moveF(boxI, boxJ)
    val nextCell = grid(boxNewI)(boxNewJ)
    if (nextCell == '.') {
      Some(boxNewI, boxNewJ)
    } else if (nextCell == 'O') {
      canShiftBoxes(grid, (boxNewI, boxNewJ), moveF)
    } else {
      None
    }
  }

  def moveOnce(
      grid: Array[Array[Char]],
      robot: (Int, Int),
      moveF: (Int, Int) => (Int, Int)
  ): (Int, Int) = {
    val (i, j) = robot
    val (nextI, nextJ) = moveF(i, j)
    val nextCell = grid(nextI)(nextJ)
    if (nextCell == '.') { // free space
      swap(grid, robot, (nextI, nextJ))
      (nextI, nextJ)
    } else if (nextCell == 'O') { // box
      val boxNewOpt = canShiftBoxes(grid, (nextI, nextJ), moveF)
      boxNewOpt match { // box can be shifted
        case Some(boxNew) =>
          swap(grid, (nextI, nextJ), boxNew)
          swap(grid, robot, (nextI, nextJ))
          (nextI, nextJ)
        case None => // box cannot be shifted
          robot
      }
    } else { // wall
      robot
    }
  }

  def move(
      grid: Array[Array[Char]],
      robot: (Int, Int),
      commands: List[Char]
  ) = {
    commands.foldLeft(robot)((prevRobot, command) =>
      moveOnce(grid, prevRobot, moveFunction(command))
    )
  }

  def part1(
      grid: Array[Array[Char]],
      commands: List[Char]
  ): Int = {
    var robot: (Int, Int) = (
      grid.indexWhere(_.contains('@')),
      grid.find(_.contains('@')).get.indexOf('@')
    )
    move(grid, robot, commands)
    grid.zipWithIndex
      .map((row, i) =>
        row.zipWithIndex
          .map((cell, j) =>
            if (cell == 'O') {
              i * 100 + j
            } else {
              0
            }
          )
          .sum
      )
      .sum
  }

  def toPart2Grid(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val newGrid: Array[Array[Char]] =
      Array.fill(grid.length)(Array.fill(2 * grid(0).length)('.'))
    grid.zipWithIndex
      .foreach((row, i) =>
        row.zipWithIndex
          .foreach((cell, j) =>
            if (cell == '@') {
              newGrid(i)(2 * j) = '@'
            } else if (cell == 'O') {
              newGrid(i)(2 * j) = '['
              newGrid(i)(2 * j + 1) = ']'
            } else if (cell == '#') {
              newGrid(i)(2 * j) = '#'
              newGrid(i)(2 * j + 1) = '#'
            }
          )
      )
    newGrid
  }
}
