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

  def findButtonFirstVertical(
      start: Location,
      end: Location
  ): List[Char] = {
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
    path.toList
  }

  def findButtonFirstHorizontal(
      start: Location,
      end: Location
  ): List[Char] = {
    val path = mutable.ListBuffer.empty[Char]
    if (end.j > start.j) {
      for (_ <- start.j until end.j) {
        path += '>'
      }
    } else if (end.j < start.j) {
      for (_ <- start.j until end.j by -1) {
        path += '<'
      }
    }

    if (end.i < start.i) {
      for (_ <- start.i until end.i by -1) {
        path += '^'
      }
    } else if (end.i > start.i) {
      for (_ <- start.i until end.i) {
        path += 'v'
      }
    }
    path.toList
  }

  def isValidPath(
      grid: Array[Array[Char]],
      start: Location,
      end: Location,
      path: List[Char]
  ): Boolean = {
    var cur = start
    var steps = List.empty[Location]
    for (move <- path) {
      cur = move match {
        case '^' => MoveNorth(cur)
        case 'v' => MoveSouth(cur)
        case '>' => MoveEast(cur)
        case '<' => MoveWest(cur)
      }
      steps = cur :: steps
    }
    steps.forall { loc =>
      validIJ(grid, loc.i, loc.j) && grid(loc.i)(loc.j) != '#'
    }
  }

  def pressButtonWays(
      grid: Array[Array[Char]],
      start: Char,
      end: Char
  ): List[List[Char]] = {
    val startLoc = findLocation(grid, start)
    val endLoc = findLocation(grid, end)
    val horizontal = findButtonFirstVertical(startLoc, endLoc)
    val vertical = findButtonFirstHorizontal(startLoc, endLoc)
    if (horizontal == vertical) {
      List(horizontal ++ List('A'))
    } else {
      val horizontalOpt =
        if (isValidPath(grid, startLoc, endLoc, horizontal)) Some(horizontal)
        else None
      val verticalOpt =
        if (isValidPath(grid, startLoc, endLoc, vertical)) Some(vertical)
        else None
      List(horizontalOpt, verticalOpt).flatten.map(_ ++ List('A'))
    }
  }

  def pressButtonsWays(
      grid: Array[Array[Char]],
      buttons: List[Char]
  ): List[List[Char]] = {
    val pairs = ('A' :: buttons).zip(buttons)
    pairs.foldLeft(List.empty[List[Char]]) {
      case (acc: List[List[Char]], pair: (Char, Char)) => {
        val (start, end) = pair
        val nextOptions = pressButtonWays(grid, start, end)
        val res = nextOptions.flatMap(next =>
          if (acc.isEmpty) List(next) else acc.map(_ ++ next)
        )
        res
      }
    }
  }

  def pressButtonsAllLayers(buttons: List[Char], maxLayer: Int): List[Char] = {
    val numericOptions = pressButtonsWays(numericGrid, buttons)
    var result: List[List[Char]] =
      numericOptions.flatMap(pressButtonsWays(directionalGrid, _))

    for (_ <- 0 until maxLayer - 1) {
      val next = result.flatMap(pressButtonsWays(directionalGrid, _))
      result = next
    }
    result.minBy(_.size)
  }

  def part1(lines: List[String], maxLayer: Int): Int = {
    lines
      .map(line =>
        pressButtonsAllLayers(line.toCharArray.toList, maxLayer).size * line
          .dropRight(1)
          .toInt
      )
      .sum
  }

  def part2(lines: List[String], maxLayer: Int): BigInt = {
    val cache = collection.mutable.Map.empty[(String, Int), Long]
    lines
      .map(line =>
        BigInt(
          pressButtonsAllLayersPart2(
            line.toCharArray.toList,
            maxLayer,
            cache
          )
        ) * BigInt(
          line
            .dropRight(1)
            .toLong
        )
      )
      .sum
  }

  // find working out in a "core moves" tests
  val bestMove: String => String = {
    case "<^A" => "v<<A>^A>A"
    case "^<A" => "<Av<A>>^A"
    case "<A"  => "v<<A>>^A"
    case "<vA" => "v<<A>A^>A"
    case "v<A" => "<vA<A>>^A"
    case "^A"  => "<A>A"
    case "vA"  => "<vA^>A"
    case "^>A" => "<Av>A^A"
    case ">^A" => "vA<^A>A"
    case ">A"  => "vA^A"
    case "v>A" => "<vA>A^A"
    case ">vA" => "vA<A^>A"
  }

  def pressButtonsAllLayersPart2(
      buttons: List[Char],
      maxLayer: Int,
      cache: mutable.Map[
        (String, Int),
        Long
      ] // buttons, layer => shortest sequence
  ): Long = {
    val numericOptions = pressButtonsWays(numericGrid, buttons)
    val secondKeypadOptions = numericOptions
      .flatMap(pressButtonsWays(directionalGrid, _))
      .map(_.mkString)
    println(
      "calculating secondKeypadOptions: " + numericOptions.map(_.mkString)
    )
    val results = secondKeypadOptions.map { option =>
      shortestSequenceAtLayer(
        option,
        2,
        maxLayer,
        cache
      )
    }
    println("")
    println("sizes: " + results)
    println("min: " + results.min)
    results.min
  }

  def shortestSequenceAtLayer(
      buttons: String,
      layer: Int,
      maxLayer: Int,
      cache: mutable.Map[
        (String, Int),
        Long
      ] // buttons, layer => shortest sequence
  ): Long = {
    if (cache.contains((buttons, layer))) {
      cache((buttons, layer))
    } else {
      val pattern = "(.)\\1+" // matches any character repeated 2 or more times
      val replacedCount = pattern.r // each repeat gives +1 to the result as it just results to A press at all keypads
        .findAllMatchIn(buttons)
        .map { m =>
          m.matched.length - 1
        }
        .sum
      val deduped = buttons.replaceAll(pattern, "$1") // no repeated characters
      val parts = deduped.split('A').map(_ + "A").map(bestMove)
      val res = if (layer == maxLayer) {
        (parts.map(_.length).sum + replacedCount).toLong
      } else {
        val results =
          parts
            .map(part => {
              shortestSequenceAtLayer(
                part,
                layer + 1,
                maxLayer,
                cache
              )
            })
        results.sum + replacedCount
      }
      cache.put((buttons, layer), res)
      res
    }
  }
}
