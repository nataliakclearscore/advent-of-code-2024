package com.advent

class Thirteen {

  def isMultipleOf(a: Long, b: Long): Boolean = a % b == 0

  // solving linear equations
  def part1Line(
      a: (Long, Long),
      b: (Long, Long),
      prize: (Long, Long)
  ): Option[(Long, Long)] = {
    val (aX, aY) = a
    val (bX, bY) = b
    val (pX, pY) = prize

    val num = aY * pX - aX * pY
    val den = aY * bX - aX * bY

    if (isMultipleOf(num, den)) {
      val y = num / den
      val x = (pX - bX * y) / aX
      Some((x, y))
    } else {
      None
    }
  }

  def part1(
      lines: List[((Long, Long), (Long, Long), (Long, Long))]
  ): Long =
    lines
      .map { case (a, b, prize) =>
        part1Line(a, b, prize)
      }
      .collect { case Some((x, y)) => x * 3 + y }
      .sum

//  def part2(
//      lines: List[((BigInt, BigInt), (BigInt, BigInt), (BigInt, BigInt))]
//  ): BigInt =
//    lines
//      .map { case (a, b, prize) =>
//        val newPrize = (
//          prize._1 + BigInt(10000000000000L),
//          prize._2 + BigInt(10000000000000L)
//        )
//        val res = part1Line(a, b, newPrize)
//        println(s"solving a: $a a: $b prise $newPrize => $res")
//        res
//      }
//      .collect { case Some((x, y)) => x * 3 + y }
//      .sum

}
