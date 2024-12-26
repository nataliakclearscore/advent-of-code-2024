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
    java.lang.Long
      .parseLong(findZValues(values, instructionList).mkString(""), 2)
  }

  def printAll(
      instructions: mutable.Map[String, Instruction]
  ): Unit = {
    var i = 0
    while (instructions.contains(f"z$i%02d")) {
      val variable = f"z$i%02d"
      val expanded = expand(instructions, variable)
      println(s"$variable = $expanded")
      i += 1
    }
  }

  def expand(
      instructions: mutable.Map[String, Instruction],
      variable: String
  ): String = {
    val instruction = instructions(variable)
    val input1: String =
      if (
        instruction.input1.startsWith("x")
        || instruction.input1.startsWith("y")
      ) {
        instruction.input1
      } else {
        s"(${expand(instructions, instruction.input1)})"
      }

    val input2: String =
      if (
        instruction.input2.startsWith("x")
        || instruction.input2.startsWith("y")
      ) {
        instruction.input2
      } else {
        s"(${expand(instructions, instruction.input2)})"
      }

    s"$input1 ${instruction.operation} $input2"
  }

  def toVar(name: String, i: Int): String = f"$name$i%02d"

  // z00 = x00 XOR y00
  // carry00 = x00 AND y00
  // z01 = x01 XOR y01 XOR carry00 =
  def createAddInstructions(): mutable.Map[String, Instruction] = {

    val instructions = mutable.Map.empty[String, Instruction]
    instructions += "z00" -> Instruction("x00", "XOR", "y00")
    instructions += "carry01" -> Instruction("x00", "AND", "y00")
    for (i <- 1 until 45) {
      // zi = xi XOR yi XOR carry(i)
      instructions += toVar("a", i) -> Instruction(
        toVar("x", i),
        "XOR",
        toVar("y", i)
      )
      instructions += toVar("z", i) -> Instruction(
        toVar("a", i),
        "XOR",
        toVar("carry", i)
      )

      // carry(i + 1) = (xi AND yi) OR ((xi XOR yi) AND carry(i))
      instructions += toVar("b", i) -> Instruction(
        toVar("x", i),
        "AND",
        toVar("y", i)
      )
      instructions += toVar("c", i) -> Instruction(
        toVar("carry", i),
        "AND",
        toVar("a", i)
      )

      if (i < 44) {
        instructions += toVar("carry", i + 1) -> Instruction(
          toVar("c", i),
          "OR",
          toVar("b", i)
        )
      } else {
        instructions += toVar("z", 45) -> Instruction(
          toVar("b", i),
          "OR",
          toVar("c", i)
        )
      }
    }
    instructions
  }

  def swap(
      instructions: mutable.Map[String, Instruction],
      k1: String,
      k2: String
  ): Unit = {
    val i1 = instructions(k1)
    val i2 = instructions(k2)
    instructions(k1) = i2
    instructions(k2) = i1
  }

  def replace(
      instructions: mutable.Map[String, Instruction],
      name: String,
      newName: String
  ): Unit = {
    println(s"Replacing $name with $newName")
    val removed = instructions.remove(name)
    removed.foreach(instructions(newName) = _)
    val keys = instructions.keys.toList
    for (key <- keys) {
      val instruction = instructions(key)
      if (instruction.input1 == name) {
        val newInstruction =
          Instruction(newName, instruction.operation, instruction.input2)
        instructions(key) = newInstruction
      }
      if (instruction.input2 == name) {
        val newInstruction =
          Instruction(instruction.input1, instruction.operation, newName)
        instructions(key) = newInstruction
      }
    }
  }

  val patternXY = """^([xy])(\d+)$""".r
  val patternCB = """^([cb])(\d+)$""".r
  val patternA = """^a(\d+)$""".r
  val patternB = """^b(\d+)$""".r
  val patternZ = """^z(\d+)$""".r
  val patternXYZ = """^([xyz])(\d+)$""".r

  // NOT solved in a general way - just found what works for the input by renaming variables and comparing input with createAddInstructions result
  def part2(instructions: mutable.Map[String, Instruction]): Unit = {

    // SWAPPING
    swap(instructions, "wjb", "cvp")
    swap(instructions, "z34", "wcb")
    swap(instructions, "z14", "qbw")
    swap(instructions, "mkk", "z10")

    var renaming: mutable.Map[String, String] = mutable.Map.empty
    var keys = instructions.keys.toList
    for (key <- keys) {
      val instruction = instructions(key)
      if (
        patternXY.matches(instruction.input1)
        && patternXY.matches(instruction.input2)
      ) {
        val i = instruction.input1.substring(1).toInt
        if (instruction.operation == "XOR" && !patternXYZ.matches(key)) {
          val newName = toVar("a", i)
          if (key != newName) {
            replace(instructions, key, newName)
            renaming(newName) = key
          }
        } else if (instruction.operation == "AND" && !patternXYZ.matches(key)) {
          val newName = toVar("b", i)
          if (key != newName) {
            replace(instructions, key, newName)
            renaming(newName) = key
          }
        }
      }
    }

    for (i <- 1 until 45) {
      val a = toVar("a", i)
      val aXorOpt = instructions.find((k, v) =>
        v.operation == "XOR" && (v.input1 == a || v.input2 == a)
      )
      val aAndOpt = instructions.find((k, v) =>
        v.operation == "AND" && (v.input1 == a || v.input2 == a)
      )
      if (aXorOpt.isEmpty || aAndOpt.isEmpty) {
        println(s"BROKEN a $a(${renaming.getOrElse(a, "")})")
      } else {
        val aXor = aXorOpt.get
        val aAnd = aAndOpt.get

        // zI = aI XOR carryI
        val carry = if (aXor._2.input1 == a) aXor._2.input2 else aXor._2.input1
        val newNameCarry = toVar("carry", i)
        replace(instructions, carry, newNameCarry)
        renaming(newNameCarry) = carry

        // cI = carryI AND aI
        val c = aAnd._1
        val newNameC = toVar("c", i)
        replace(instructions, c, newNameC)
        renaming(newNameC) = c
      }
    }

    for (i <- 1 until 45) {
      val b = toVar("b", i)
      val aOrOpt = instructions.find((k, v) =>
        v.operation == "OR" && (v.input1 == b || v.input2 == b)
      )
      if (aOrOpt.isEmpty) {
        println(s"BROKEN b $b(${renaming.getOrElse(b, "")})")
      }
    }

    println("--------------------")
    for ((k, v) <- instructions) {
      println(s"$k = ${v.input1} ${v.operation} ${v.input2}")
    }

    println("--------------------")
    val correctInstructions = createAddInstructions()
    for ((k, v) <- instructions) {
      if (!correctInstructions.contains(k)) {
        println(s"$k")
      } else {
        val w = correctInstructions(k)
        if (w.operation != v.operation) {
          println(s"$k(${renaming.getOrElse(k, "")})")
        }
      }
    }
  }
}
