package com.advent

class TwentyThree {

  def findTriples(
      pairs: List[(String, String)]
  ): List[(String, String, String)] = {
    val pairMap = scala.collection.mutable.Map.empty[String, Set[String]]
    val triples =
      scala.collection.mutable.Set.empty[(String, String, String)]

    pairs.foreach { case (a, b) =>
      val aSet = pairMap.getOrElse(a, Set.empty)
      val bSet = pairMap.getOrElse(b, Set.empty)

      aSet.intersect(bSet).foreach { c =>
        triples += ((a, b, c))
//        val sorted = List(a, b, c).sorted
//        triples += ((sorted(0), sorted(1), sorted(2)))
      }

      pairMap(a) = aSet + b
      pairMap(b) = bSet + a
    }
    triples.toList
  }

  def part1(pairs: List[(String, String)]): Int = {
    val triples = findTriples(pairs)
    triples.count(t =>
      t._1.startsWith("t") || t._2.startsWith("t") || t._3.startsWith("t")
    )
  }
}
