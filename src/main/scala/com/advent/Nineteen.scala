package com.advent

case class Node(
    var canBeTerminal: Boolean = false,
    next: scala.collection.mutable.Map[Char, Node] =
      scala.collection.mutable.Map.empty
)

class Nineteen {

  def possibleDesign(
      patterns: List[String],
      design: String,
      start: Int
  ): Boolean = {
    if (start >= design.length) {
      false
    } else {
      val canContinue =
        patterns.filter(pattern => design.startsWith(pattern, start))
      canContinue.exists(nextPattern =>
        design.substring(start) == nextPattern || possibleDesign(
          patterns,
          design,
          start + nextPattern.length
        )
      )
    }
  }

  def part1(patterns: List[String], designs: List[String]): Int = {
    designs.map(design => possibleDesign(patterns, design, 0)).count(identity)
  }

  def buildTrie(patterns: List[String]): Node = {
    val root = Node()
    patterns.foreach(pattern => {
      val chars = pattern.toCharArray.toList
      var current = root
      for (i <- chars.indices) {
        val char = chars(i)
        val next = if (current.next.contains(char)) {
          current = current.next(char)
        } else {
          val newNode = Node()
          current.next.put(char, newNode)
          current = newNode
        }
      }
      current.canBeTerminal = true
    })
    root
  }

  def findDesignsUnoptimal(
      patterns: List[String],
      design: String,
      start: Int
  ): List[List[String]] = {
    if (start >= design.length) {
      List.empty
    } else {
      val canContinue =
        patterns.filter(pattern => design.startsWith(pattern, start))
      canContinue.flatMap(nextPattern => {
        if (design.substring(start) == nextPattern) {
          List(List(nextPattern))
        } else {
          findDesignsUnoptimal(patterns, design, start + nextPattern.length)
            .map(nextPattern :: _)
        }
      })
    }
  }

  def findContinuations(
      trie: Node,
      toMatch: String,
      i: Int
  ): List[List[Char]] = {
    if (i >= toMatch.length) {
      List.empty
    } else {
      val currentChar = toMatch(i)
      if (trie.next.contains(currentChar)) {
        val nextNode = trie.next(currentChar)
        val newEndingOpt = if (nextNode.canBeTerminal) {
          Some(List(currentChar))
        } else {
          None
        }
        newEndingOpt.toList ++ findContinuations(nextNode, toMatch, i + 1).map(
          currentChar :: _
        )
      } else {
        List.empty
      }
    }
  }

  def findContinuations(
      trie: Node,
      toMatch: String
  ): List[String] = {
    findContinuations(trie, toMatch, 0).map(_.mkString)
  }

  def countDesigns(
      trie: Node,
      design: String,
      start: Int,
      cache: scala.collection.mutable.Map[String, Long]
  ): Long = {
    if (start >= design.length) {
      0
    } else {
      val substring = design.substring(start)
      if (cache.contains(substring)) {
        cache(substring)
      } else {
        val canContinue = findContinuations(trie, substring)
        val result = canContinue
          .map(nextPattern => {
            if (design.length - start == nextPattern.length) {
              1
            } else {
              countDesigns(
                trie,
                design,
                start + nextPattern.length,
                cache
              )
            }
          })
          .sum
        if (substring.length >= 4) {
          cache.put(substring, result)
        }
        result
      }
    }
  }

  def part2(patterns: List[String], designs: List[String]): Long = {
    val trie = buildTrie(patterns)
    val cache = scala.collection.mutable.Map.empty[String, Long]
    designs
      .map(countDesigns(trie, _, 0, cache))
      .sum
  }
}
