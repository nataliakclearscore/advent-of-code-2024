package com.advent

class Thirteen {

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

    if (num % den == 0) {
      val y = num / den
      if ((pX - bX * y) % aX == 0) {
        val x = (pX - bX * y) / aX
        Some((x, y))
      } else {
        None
      }
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

  def part2(
      lines: List[((Long, Long), (Long, Long), (Long, Long))]
  ): BigInt =
    lines
      .map { case (a, b, prize) =>
        val newPrize = (
          prize._1 + 10_000_000_000_000L,
          prize._2 + 10_000_000_000_000L
        )
        part1Line(a, b, newPrize)
      }
      .collect { case Some((x, y)) => x * 3 + y }
      .sum
}
