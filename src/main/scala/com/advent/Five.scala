package com.advent

class Five {
  def isOrderRight(list: List[Int], rules: List[(Int, Int)]): Boolean = {
    val map: Map[Int, Int] = list.zipWithIndex.toMap
    rules.forall { case (a, b) =>
      if (map.contains(a) && map.contains(b))
        map(a) < map(b)
      else true
    }
  }

  def part1(lists: List[List[Int]], rules: List[(Int, Int)]): Int = {
    lists.foldLeft(0)((acc, list) => if (isOrderRight(list, rules)) acc + list(list.length / 2) else acc)
  }
}
