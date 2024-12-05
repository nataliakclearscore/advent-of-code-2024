package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

def parse(input: String): (List[(Int, Int)], List[List[Int]]) = {
  val lines = input.split("\n")
  val split = lines.indexOf("")
  val rules: List[(Int, Int)] = lines
    .slice(0, split)
    .map(str => {
      val a = str.split("\\|").map(_.toInt)
      (a(0), a(1))
    })
    .toList
  val lists: List[List[Int]] = lines
    .slice(split + 1, lines.length)
    .map(_.split(",").map(_.toInt).toList)
    .toList
  (rules, lists)
}

class FiveTest extends AnyWordSpec with should.Matchers {

  "isOrderRight" should {
    "work for an example" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin
      val (rules, lists) = parse(input)
      lists.map(new Five().isOrderRight(_, rules)) shouldEqual List(
        true,
        true,
        true,
        false,
        false,
        false
      )
    }
  }

  "part1" should {
    "work for an example" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin
      val (rules, lists) = parse(input)
      new Five().part1(lists, rules) shouldEqual 143
    }
  }

  "order" should {
    "do nothing for lists the right order" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin
      val (rules, lists) = parse(input)
      new Five().order(lists(0), rules) shouldEqual List(75, 47, 61, 53, 29)
      new Five().order(lists(1), rules) shouldEqual List(97, 61, 53, 29, 13)
      new Five().order(lists(2), rules) shouldEqual List(75, 29, 13)
    }

    "order lists that are not in the right order" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin
      val (rules, lists) = parse(input)
      new Five().order(lists(3), rules) shouldEqual List(97, 75, 47, 61, 53)
      new Five().order(lists(4), rules) shouldEqual List(61,29,13)
      new Five().order(lists(5), rules) shouldEqual List(97,75,47,29,13)
    }
  }

  "part2" should {
    "work for an example" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin
      val (rules, lists) = parse(input)
      new Five().part2(lists, rules) shouldEqual 123
    }
  }
}
