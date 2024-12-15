package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class FifteenTest extends AnyWordSpec with should.Matchers {

  def parse(input: String): (Array[Array[Char]], List[Char]) = {
    val sections = input.split("\n\n")
    val grid: Array[Array[Char]] = sections(0).split("\n").map(_.toCharArray)
    val commands: List[Char] =
      sections(1).split("\n").toList.flatMap(_.trim.toCharArray.toList)
    (grid, commands)
  }

  val underTest = new Fifteen

  "example 1" should {

    val input =
      """########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |
        |<^^>>>vv<v>>v<<""".stripMargin

    "move" in {
      val (grid, commands) = parse(input)
      var robot: (Int, Int) = (
        grid.indexWhere(_.contains('@')),
        grid.find(_.contains('@')).get.indexOf('@')
      )
      robot = underTest.move(grid, robot, commands)
      val newGrid =
        """########
          |#....OO#
          |##.....#
          |#.....O#
          |#.#O@..#
          |#...O..#
          |#...O..#
          |########""".stripMargin.split("\n").map(_.toCharArray)
      grid shouldBe newGrid
    }

    "part1" in {
      val (grid, commands) = parse(input)
      underTest.part1(grid, commands) shouldEqual 2028
    }
  }

  "example 2" should {
    val input =
      """##########
          |#..O..O.O#
          |#......O.#
          |#.OO..O.O#
          |#..O@..O.#
          |#O#..O...#
          |#O..O..O.#
          |#.OO.O.OO#
          |#....O...#
          |##########
          |
          |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
          |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
          |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
          |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
          |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
          |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
          |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
          |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
          |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
          |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

    "move" in {
      val (grid, commands) = parse(input)
      var robot: (Int, Int) = (
        grid.indexWhere(_.contains('@')),
        grid.find(_.contains('@')).get.indexOf('@')
      )
      robot = underTest.move(grid, robot, commands)
      val newGrid =
        """##########
            |#.O.O.OOO#
            |#........#
            |#OO......#
            |#OO@.....#
            |#O#.....O#
            |#O.....OO#
            |#O.....OO#
            |#OO....OO#
            |##########""".stripMargin.split("\n").map(_.toCharArray)
      grid shouldBe newGrid
    }

    "part1" in {
      val (grid, commands) = parse(input)
      underTest.part1(grid, commands) shouldEqual 10092
    }

    "toPart2Grid" in {
      val (grid, commands) = parse(input)
      val part2Grid = underTest.toPart2Grid(grid)

    }

    "moveOncePart2 horizontal" in {
      val (grid, commands) = parse(input)
      val part2Grid = underTest.toPart2Grid(grid)
      var robot: (Int, Int) = (
        part2Grid.indexWhere(_.contains('@')),
        part2Grid.find(_.contains('@')).get.indexOf('@')
      )

      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('^'))
      for (i <- 0 until 10) {
        robot =
          underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('>'))
      }
      part2Grid shouldBe
        """####################
          |##....[]....[]..[]##
          |##............[]..##
          |##..[][].....@[][]##
          |##....[]......[]..##
          |##[]##....[]......##
          |##[]....[]....[]..##
          |##..[][]..[]..[][]##
          |##........[]......##
          |####################""".stripMargin
          .split("\n")
          .map(_.toCharArray)

      for (i <- 0 until 10) {
        robot =
          underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('<'))
      }
      part2Grid shouldBe
        """####################
          |##....[]....[]..[]##
          |##............[]..##
          |##[][]@.......[][]##
          |##....[]......[]..##
          |##[]##....[]......##
          |##[]....[]....[]..##
          |##..[][]..[]..[][]##
          |##........[]......##
          |####################""".stripMargin
          .split("\n")
          .map(_.toCharArray)
    }

    "move part 2 vertical" in {
      val (grid, commands) = parse(input)
      val part2Grid = underTest.toPart2Grid(grid)
      var robot: (Int, Int) = (
        part2Grid.indexWhere(_.contains('@')),
        part2Grid.find(_.contains('@')).get.indexOf('@')
      )

      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('>'))
      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('>'))
      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('v'))
      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('v'))
      part2Grid shouldBe
        """####################
          |##....[]....[]..[]##
          |##............[]..##
          |##..[][]....[]..[]##
          |##....[]......[]..##
          |##[]##....@.......##
          |##[]....[][]..[]..##
          |##..[][]..[]..[][]##
          |##........[]......##
          |####################""".stripMargin
          .split("\n")
          .map(_.toCharArray)
    }

    "move part 2" in {
      val (grid, commands) = parse(input)
      val part2Grid = underTest.toPart2Grid(grid)

      var robot: (Int, Int) = (
        part2Grid.indexWhere(_.contains('@')),
        part2Grid.find(_.contains('@')).get.indexOf('@')
      )
      robot = underTest.movePart2(part2Grid, robot, commands)
      val newGrid =
        """####################
          |##[].......[].[][]##
          |##[]...........[].##
          |##[]........[][][]##
          |##[]......[]....[]##
          |##..##......[]....##
          |##..[]............##
          |##..@......[].[][]##
          |##......[][]..[]..##
          |####################""".stripMargin.split("\n").map(_.toCharArray)
      part2Grid shouldBe newGrid
    }

    "part2" in {
      val (grid, commands) = parse(input)
      underTest.part2(grid, commands) shouldEqual 9021
    }
  }

  "example 3" should {
    val input =
      """##############
        |##......##..##
        |##..........##
        |##...[][]...##
        |##....[]....##
        |##.....@....##
        |##############""".stripMargin

    "move vertical multi blocks" in {
      val part2Grid = input.split("\n").map(_.toCharArray)
      var robot: (Int, Int) = (
        part2Grid.indexWhere(_.contains('@')),
        part2Grid.find(_.contains('@')).get.indexOf('@')
      )

      part2Grid.foreach(row => println(row.mkString))

      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('^'))

      part2Grid.foreach(row => println(row.mkString))
      part2Grid shouldBe
        """##############
          |##......##..##
          |##...[][]...##
          |##....[]....##
          |##.....@....##
          |##..........##
          |##############""".stripMargin
          .split("\n")
          .map(_.toCharArray)

      robot =
        underTest.moveOncePart2(part2Grid, robot, underTest.moveFunction('^'))

      part2Grid.foreach(row => println(row.mkString))
      part2Grid shouldBe
        """##############
          |##......##..##
          |##...[][]...##
          |##....[]....##
          |##.....@....##
          |##..........##
          |##############""".stripMargin
          .split("\n")
          .map(_.toCharArray)
    }
  }
}
