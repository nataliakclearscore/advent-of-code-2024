package com.advent

class Five {

  // part 1
  def isOrderRight(list: List[Int], rules: List[(Int, Int)]): Boolean = {
    val map: Map[Int, Int] = list.zipWithIndex.toMap
    rules.forall { case (a, b) =>
      if (map.contains(a) && map.contains(b))
        map(a) < map(b)
      else true
    }
  }

  def part1(lists: List[List[Int]], rules: List[(Int, Int)]): Int = {
    lists.foldLeft(0)((acc, list) =>
      if (isOrderRight(list, rules)) acc + list(list.length / 2) else acc
    )
  }

  // part 2
  def order(list: List[Int], rules: List[(Int, Int)]): List[Int] = {
    val rulesMap: Map[Int, List[Int]] =
      rules.groupBy(_._1).map((first, seconds) => (first, seconds.map(_._2)))
    val arr = list.toArray
    var i = 1
    while (i < arr.length) {
      val bubble = arr(i)
      var j = i - 1
      while (j >= 0 && !rulesMap.get(arr(j)).exists(_.contains(bubble))) {
        val tmp = arr(j)
        arr(j) = bubble
        arr(j + 1) = tmp
        j -= 1
      }
      i += 1
    }
    arr.toList
  }

  def part2(lists: List[List[Int]], rules: List[(Int, Int)]): Int = {
    lists.foldLeft(0)((acc, list) =>
      if (!isOrderRight(list, rules)) {
        val ordered = order(list, rules)
        acc + ordered(ordered.length / 2)
      } else acc
    )
  }
}
