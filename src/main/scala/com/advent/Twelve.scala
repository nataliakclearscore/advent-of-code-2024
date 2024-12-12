package com.advent

class Twelve {

  val Horizontal1 = (loc: (Int, Int)) => (loc._1, loc._2 + 1)
  val Horizontal2 = (loc: (Int, Int)) => (loc._1, loc._2 - 1)
  val Vertical1 = (loc: (Int, Int)) => (loc._1 + 1, loc._2)
  val Vertical2 = (loc: (Int, Int)) => (loc._1 - 1, loc._2)
  val Diagonal1 = (loc: (Int, Int)) => (loc._1 + 1, loc._2 + 1)
  val Diagonal2 = (loc: (Int, Int)) => (loc._1 - 1, loc._2 - 1)
  val Diagonal3 = (loc: (Int, Int)) => (loc._1 + 1, loc._2 - 1)
  val Diagonal4 = (loc: (Int, Int)) => (loc._1 - 1, loc._2 + 1)

  def validLoc(arr: Array[Array[Char]], loc: (Int, Int)): Boolean = {
    val (x, y) = loc
    x >= 0 & y >= 0 & x < arr.length & y < arr(0).length
  }

  def buildRegion(
      map: Array[Array[Char]],
      track: Char,
      loc: (Int, Int),
      visited: Array[Array[Boolean]]
  ): List[(Int, Int)] = {
    val (x, y) = loc
    if (!validLoc(map, loc) || visited(x)(y) || map(x)(y) != track) {
      Nil
    } else {
      visited(x)(y) = true
      loc ::
        buildRegion(map, track, Horizontal1(loc), visited) ++
        buildRegion(map, track, Horizontal2(loc), visited) ++
        buildRegion(map, track, Vertical1(loc), visited) ++
        buildRegion(map, track, Vertical2(loc), visited)
    }
  }

  def calcPerimeter(
      map: Array[Array[Char]],
      region: List[(Int, Int)],
      track: Char
  ): Int = {
    // count all sides that are not in the region
    region
      .map(loc => {
        val left = Horizontal2(loc)
        val right = Horizontal1(loc)
        val up = Vertical2(loc)
        val down = Vertical1(loc)
        List(
          !validLoc(map, left) || map(left._1)(left._2) != track,
          !validLoc(map, right) || map(right._1)(right._2) != track,
          !validLoc(map, up) || map(up._1)(up._2) != track,
          !validLoc(map, down) || map(down._1)(down._2) != track
        ).count(identity)
      })
      .sum
  }

  def part1(map: Array[Array[Char]]) = {
    val visited: Array[Array[Boolean]] = Array.ofDim(map.length, map(0).length)
    var regions: List[(Char, List[(Int, Int)])] = List.empty
    for (i <- map.indices) {
      for (j <- map(0).indices) {
        if (!visited(i)(j)) {
          val cur = map(i)(j)
          val coords = buildRegion(map, cur, (i, j), visited)
          regions = (cur, coords) :: regions
        }
      }
    }

    regions
      .map(region => {
        val (track, coords) = region
        val area = coords.size
        val perimeter = calcPerimeter(map, coords, track)
        area * perimeter
      })
      .sum
  }

  def matches(map: Array[Array[Char]], loc: (Int, Int), track: Char) =
    validLoc(map, loc) && map(loc._1)(loc._2) == track

  def calcCorners(
      map: Array[Array[Char]],
      region: List[(Int, Int)],
      track: Char
  ): Int = {
    // count all sides that are not in the region
    region
      .map(loc => {
        val left = Horizontal2(loc)
        val right = Horizontal1(loc)
        val up = Vertical2(loc)
        val down = Vertical1(loc)
        val corners = List(
          // outer corners
          !matches(map, left, track) && !matches(map, up, track),
          !matches(map, right, track) && !matches(map, up, track),
          !matches(map, right, track) && !matches(map, down, track),
          !matches(map, left, track) && !matches(map, down, track),
          // inner corners
          matches(map, left, track) && matches(map, up, track) && !matches(
            map,
            Diagonal2(loc),
            track
          ),
          matches(map, right, track) && matches(map, up, track) && !matches(
            map,
            Diagonal4(loc),
            track
          ),
          matches(map, right, track) && matches(map, down, track) && !matches(
            map,
            Diagonal1(loc),
            track
          ),
          matches(map, left, track) && matches(map, down, track) && !matches(
            map,
            Diagonal3(loc),
            track
          )
        )
        corners.count(identity)
      })
      .sum
  }

  def part2(map: Array[Array[Char]]) = {
    val visited: Array[Array[Boolean]] = Array.ofDim(map.length, map(0).length)
    var regions: List[(Char, List[(Int, Int)])] = List.empty
    for (i <- map.indices) {
      for (j <- map(0).indices) {
        if (!visited(i)(j)) {
          val cur = map(i)(j)
          val coords = buildRegion(map, cur, (i, j), visited)
          regions = (cur, coords) :: regions
        }
      }
    }

    regions
      .map(region => {
        val (track, coords) = region
        val area = coords.size
        val perimeter = calcCorners(map, coords, track) // THE ONLY CHANGE
        area * perimeter
      })
      .sum
  }
}
