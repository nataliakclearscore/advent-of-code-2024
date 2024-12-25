package com.advent

import scala.collection.mutable

case class Instruction(input1: String, operation: String, input2: String)

class TwentyFour {

  def calculate(
      allValues: mutable.Map[String, Int],
      instructions: mutable.Map[String, Instruction],
      variable: String
  ): Int = {
    val instruction = instructions(variable)
    val input1 = allValues.getOrElseUpdate(
      instruction.input1,
      calculate(allValues, instructions, instruction.input1)
    )
    val input2 = allValues.getOrElseUpdate(
      instruction.input2,
      calculate(allValues, instructions, instruction.input2)
    )
    val result = instruction.operation match {
      case "AND" => input1 & input2
      case "OR"  => input1 | input2
      case "XOR" => input1 ^ input2
      case _ =>
        throw new IllegalArgumentException(
          s"Invalid operation: ${instruction.operation}"
        )
    }
    allValues(variable) = result
    result
  }

  def findZValues(
      values: Map[String, Int],
      instructionList: List[(String, String, String, String)]
  ): List[Int] = {
    val allValues = mutable.Map.empty[String, Int]
    allValues ++= values

    val instructions: mutable.Map[String, Instruction] =
      mutable.Map.empty[String, Instruction]
    instructionList.foreach { instruction =>
      instructions += instruction._4 -> Instruction(
        instruction._1,
        instruction._2,
        instruction._3
      )
    }

    var i = 0
    var result = List.empty[Int]
    while (instructions.contains(f"z$i%02d")) {
      val variable = f"z$i%02d"
      result = calculate(allValues, instructions, variable) :: result
      i += 1
    }
    result
  }

  def part1(
      values: Map[String, Int],
      instructionList: List[(String, String, String, String)]
  ): Long = {
    java.lang.Long.parseLong(findZValues(values, instructionList).mkString(""), 2)
  }
}
