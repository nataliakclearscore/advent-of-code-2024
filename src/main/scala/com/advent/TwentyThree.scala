package com.advent

class TwentyThree {

  def findTriples(
      pairs: List[(String, String)]
  ): List[(String, String, String)] = {
    val connections = scala.collection.mutable.Map.empty[String, Set[String]]
    val triples =
      scala.collection.mutable.Set.empty[(String, String, String)]

    pairs.foreach { case (a, b) =>
      val aSet = connections.getOrElse(a, Set.empty)
      val bSet = connections.getOrElse(b, Set.empty)

      aSet.intersect(bSet).foreach { c =>
        triples += ((a, b, c))
      }
      connections(a) = aSet + b
      connections(b) = bSet + a
    }

    triples.toList
  }

  def part1(pairs: List[(String, String)]): Int = {
    val triples = findTriples(pairs)
    triples.count(t =>
      t._1.startsWith("t") || t._2.startsWith("t") || t._3.startsWith("t")
    )
  }

  def findNetworksLevelN(
      prevLevel: List[Set[String]],
      connections: Map[String, Set[String]],
      n: Int
  ): List[Set[String]] = {
    val nextLevel = scala.collection.mutable.Set.empty[Set[String]]
    for (prev <- prevLevel) {
      prev.toList
        .map(connections)
        .reduce(_ intersect _)
        .foreach(intersection => {
          val candidate = prev + intersection
          nextLevel += candidate
        })
    }
    nextLevel.toList
  }

  def part2(pairs: List[(String, String)]): String = {
    val tmp = scala.collection.mutable.Map.empty[String, Set[String]]
    pairs.foreach { case (a, b) =>
      tmp(a) = tmp.getOrElse(a, Set.empty) + b
      tmp(b) = tmp.getOrElse(b, Set.empty) + a
    }
    val connections = tmp.toMap

    var curentLevel = pairs.map(p => Set(p._1, p._2))
    var nextLevel = findNetworksLevelN(curentLevel, connections, 2)
    while (nextLevel.nonEmpty) {
      curentLevel = nextLevel
      nextLevel = findNetworksLevelN(curentLevel, connections, 2)
    }
    curentLevel.head.toList.sorted.mkString(",")
  }
}
