package com.advent

class Seventeen {

  def process(
      a0: Int,
      b0: Int,
      c0: Int,
      program: List[Int]
  ): (Int, Int, Int, List[Int]) = {
    var a = a0
    var b = b0
    var c = c0
    val output = scala.collection.mutable.ListBuffer.empty[Int]

    def toOperand(operandCode: Int): Int = operandCode match {
      case x if x >= 0 && x <= 3 => x
      case 4                     => a
      case 5                     => b
      case 6                     => c
      case _ =>
        throw new IllegalArgumentException(
          s"Invalid operand code: $operandCode"
        )
    }

    var i = 0
    while (i < program.length - 1) {
      val opcode = program(i)
      val literalOperand = program(i + 1)
      val comboOperand = toOperand(literalOperand)
      opcode match {
        case 0 =>
          // a = a / Math.pow(2, operand).toInt
          val res = BigInt(a) >> comboOperand
          a = res.toInt
          i += 2
        case 1 =>
          val res = b ^ literalOperand
          b = res
          i += 2
        case 2 =>
          val res = comboOperand & 0x07
          b = res
          i += 2
        case 3 =>
          if (a > 0) {
            i = literalOperand
          } else {
            i += 2
          }
        case 4 =>
          val res = b ^ c
          b = res
          i += 2
        case 5 =>
          val res = comboOperand & 0x07
          output += res
          i += 2
        case 6 =>
          val res = BigInt(a) >> comboOperand
          b = res.toInt
          i += 2
        case 7 =>
          val res = BigInt(a) >> comboOperand
          c = res.toInt
          i += 2
        case _ =>
          throw new IllegalArgumentException(
            s"Invalid opcode code: $opcode"
          )
      }
    }
    (a, b, c, output.toList)
  }

  def part1(
      a0: Int,
      b0: Int,
      c0: Int,
      program: List[Int]
  ): String = {
    process(a0, b0, c0, program)._4.mkString(",")
  }
}
