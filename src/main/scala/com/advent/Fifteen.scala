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

  def canShiftBoxesPart2Horizontal(
      grid: Array[Array[Char]],
      box: (Int, Int),
      moveF: (Int, Int) => (Int, Int)
  ): Option[(Int, Int)] = {
    val (boxI, boxJ) = box
    val (boxNewI, boxNewJ) = moveF(boxI, boxJ)
    val nextCell = grid(boxNewI)(boxNewJ)
    if (nextCell == '.') {
      Some(boxNewI, boxNewJ)
    } else if (nextCell == '[' || nextCell == ']') {
      canShiftBoxesPart2Horizontal(grid, (boxNewI, boxNewJ), moveF)
    } else {
      None
    }
  }

  def canShiftBoxesPart2Vertical(
      grid: Array[Array[Char]],
      box: (Int, Int),
      moveF: (Int, Int) => (Int, Int)
  ): (Boolean, List[(Int, Int, Char)]) = {
    var result = true
    val (boxI, boxJ) = box
    val queue = scala.collection.mutable.Queue[(Int, Int)](box)
    val toShift = scala.collection.mutable
      .ListBuffer[(Int, Int, Char)]((boxI, boxJ, grid(boxI)(boxJ)))

    if (grid(boxI)(boxJ) == '[') {
      queue.enqueue((boxI, boxJ + 1))
      toShift.append((boxI, boxJ + 1, ']'))
    } else {
      queue.enqueue((boxI, boxJ - 1))
      toShift.append((boxI, boxJ - 1, '['))
    }

    while (result && queue.nonEmpty) {
      val (i, j) = queue.dequeue()
      val (nextI, nextJ) = moveF(i, j)
      val nextCell = grid(nextI)(nextJ)
      if (nextCell == '#') { // hit the wall
        result = false
      } else if (nextCell == '[') {
        queue.enqueue((nextI, nextJ))
        toShift.append((nextI, nextJ, '['))

        queue.enqueue((nextI, nextJ + 1))
        toShift.append((nextI, nextJ + 1, ']'))
      } else if (nextCell == ']') {
        queue.enqueue((nextI, nextJ))
        toShift.append((nextI, nextJ, ']'))

        queue.enqueue((nextI, nextJ - 1))
        toShift.append((nextI, nextJ - 1, '['))
      }
    }

    (result, toShift.toList)
  }

  def moveOncePart2(
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
    } else if (nextCell == '[' || nextCell == ']') { // box
      if (moveF == Horizontal1 || moveF == Horizontal2) { // horizontal move
        val boxNewOpt =
          canShiftBoxesPart2Horizontal(grid, (nextI, nextJ), moveF)
        boxNewOpt match { // box can be shifted
          case Some(boxNew) =>
            if (moveF == Horizontal1) {
              for (k <- boxNew._2 until nextJ by -1) {
                swap(grid, (nextI, k), (nextI, k - 1))
              }
            } else {
              for (k <- boxNew._2 until nextJ) {
                swap(grid, (nextI, k), (nextI, k + 1))
              }
            }
            swap(grid, robot, (nextI, nextJ))
            (nextI, nextJ)
          case None => // box cannot be shifted
            robot
        }
      } else if (moveF == Vertical1 || moveF == Vertical2) { // vertical move
        val (canShift, toShift) =
          canShiftBoxesPart2Vertical(grid, (nextI, nextJ), moveF)
        if (canShift) {
          toShift.foreach((i, j, _) => {
            grid(i)(j) = '.'
          })
          toShift.foreach((i, j, c) => {
            val (nextI, nextJ) = moveF(i, j)
            grid(nextI)(nextJ) = c
          })
          swap(grid, robot, (nextI, nextJ))
          (nextI, nextJ)
        } else {
          robot
        }
      } else {
        robot
      }
    } else { // wall
      robot
    }
  }

  def movePart2(
      grid: Array[Array[Char]],
      robot: (Int, Int),
      commands: List[Char]
  ) = {
    commands.foldLeft(robot)((prevRobot, command) =>
      moveOncePart2(grid, prevRobot, moveFunction(command))
    )
  }

  def part2(
      grid: Array[Array[Char]],
      commands: List[Char]
  ): Int = {
    val part2Grid = toPart2Grid(grid)
    val robot: (Int, Int) = (
      part2Grid.indexWhere(_.contains('@')),
      part2Grid.find(_.contains('@')).get.indexOf('@')
    )
    movePart2(part2Grid, robot, commands)
    part2Grid.zipWithIndex
      .map((row, i) =>
        row.zipWithIndex
          .map((cell, j) =>
            if (cell == '[') {
              i * 100 + j
            } else {
              0
            }
          )
          .sum
      )
      .sum
  }
}
