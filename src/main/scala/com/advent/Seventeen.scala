package com.advent

class Seventeen {

  def process(
      a0: BigInt,
      b0: BigInt,
      c0: BigInt,
      program: List[Int]
  ): (BigInt, BigInt, BigInt, List[Int]) = {
    var a = a0
    var b = b0
    var c = c0
    val output = scala.collection.mutable.ListBuffer.empty[Int]

    def toOperand(operandCode: Int): BigInt = operandCode match {
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
          //val res = a / Math.pow(2, comboOperand)
          val res =
            if (comboOperand.isValidInt) a >> comboOperand.toInt
            else BigInt(0)
          //val res = BigInt(a) >> comboOperand
          a = res
          i += 2
        case 1 =>
          val res = b ^ BigInt(literalOperand)
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
          output += res.toInt
          i += 2
        case 6 =>
          val res =
            if (comboOperand.isValidInt) a >> comboOperand.toInt
            else BigInt(0)
          //val res = BigInt(a) >> comboOperand
          b = res
          i += 2
        case 7 =>
          val res =
            if (comboOperand.isValidInt) a >> comboOperand.toInt
            else BigInt(0)
          //val res = BigInt(a) >> comboOperand
          c = res
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
      a0: BigInt,
      b0: BigInt,
      c0: BigInt,
      program: List[Int]
  ): String = {
    process(a0, b0, c0, program)._4.mkString(",")
  }

  def octalToDecimal(oct: String): BigInt = {
    BigInt(oct, 8)
  }

  def part2(program: List[Int]): BigInt = {
    val expected = program.mkString(",")
    var octNumStrings: List[String] = List("3")
    for (n <- 1 until program.size) {
      var i = 0
      var newOctNumStrings: List[String] = List.empty
      for (i <- 0 to 7) {
        for (octNumStr <- octNumStrings) {
          val testNumber = octNumStr + i
          val testResult = part1(
            octalToDecimal(testNumber).toLong,
            0,
            0,
            program
          )
          if (expected.endsWith(testResult)) {
            newOctNumStrings = testNumber :: newOctNumStrings
          }
        }
      }
      octNumStrings = newOctNumStrings.toList
    }
    octNumStrings.map(octalToDecimal).min
  }
}
