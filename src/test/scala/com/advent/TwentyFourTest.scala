package com.advent

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.util.matching.Regex

class TwentyFourTest extends AnyWordSpec with should.Matchers {

  private def parse(input: String) = {
    val lines: List[String] = input.split("\\r?\\n").toList

    val valuePattern: Regex = """^([xy]\d{2}):\s*(\d+)$""".r

    val instructionPattern: Regex =
      """^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s*->\s*(\w+)$""".r

    val assignments: Map[String, Int] = lines.collect {
      case valuePattern(varName, valueStr) =>
        varName -> valueStr.toInt
    }.toMap

    val instructions: List[(String, String, String, String)] = lines.collect {
      case instructionPattern(lhs, op, rhs, out) =>
        (lhs, op, rhs, out)
    }

    (assignments, instructions)
  }

  private val underTest = new TwentyFour

  "test" should {

    "operations" in {
      1 & 1 shouldEqual 1
      1 & 0 shouldEqual 0
      0 & 1 shouldEqual 0
      0 & 0 shouldEqual 0

      1 | 1 shouldEqual 1
      1 | 0 shouldEqual 1
      0 | 1 shouldEqual 1
      0 | 0 shouldEqual 0

      1 ^ 1 shouldEqual 0
      1 ^ 0 shouldEqual 1
      0 ^ 1 shouldEqual 1
      0 ^ 0 shouldEqual 0
    }
  }

  "example 1" should {

    val input =
      """x00: 1
        |x01: 1
        |x02: 1
        |y00: 0
        |y01: 1
        |y02: 0
        |
        |x00 AND y00 -> z00
        |x01 XOR y01 -> z01
        |x02 OR y02 -> z02""".stripMargin

    "find z values" in {
      val (assignments, instructions) = parse(input)
      val result =
        underTest.findZValues(assignments, instructions) shouldBe List(1, 0, 0)
    }

    "part 1" in {
      val (assignments, instructions) = parse(input)
      val result = underTest.part1(assignments, instructions) shouldBe 4
    }
  }

  "example 2" should {

    val input =
      """x00: 1
        |x01: 0
        |x02: 1
        |x03: 1
        |x04: 0
        |y00: 1
        |y01: 1
        |y02: 1
        |y03: 1
        |y04: 1
        |
        |ntg XOR fgs -> mjb
        |y02 OR x01 -> tnw
        |kwq OR kpj -> z05
        |x00 OR x03 -> fst
        |tgd XOR rvg -> z01
        |vdt OR tnw -> bfw
        |bfw AND frj -> z10
        |ffh OR nrd -> bqk
        |y00 AND y03 -> djm
        |y03 OR y00 -> psh
        |bqk OR frj -> z08
        |tnw OR fst -> frj
        |gnj AND tgd -> z11
        |bfw XOR mjb -> z00
        |x03 OR x00 -> vdt
        |gnj AND wpb -> z02
        |x04 AND y00 -> kjc
        |djm OR pbm -> qhw
        |nrd AND vdt -> hwm
        |kjc AND fst -> rvg
        |y04 OR y02 -> fgs
        |y01 AND x02 -> pbm
        |ntg OR kjc -> kwq
        |psh XOR fgs -> tgd
        |qhw XOR tgd -> z09
        |pbm OR djm -> kpj
        |x03 XOR y03 -> ffh
        |x00 XOR y04 -> ntg
        |bfw OR bqk -> z06
        |nrd XOR fgs -> wpb
        |frj XOR qhw -> z04
        |bqk OR frj -> z07
        |y03 OR x01 -> nrd
        |hwm AND bqk -> z03
        |tgd XOR rvg -> z12
        |tnw OR pbm -> gnj""".stripMargin

    "find z values" in {
      val (assignments, instructions) = parse(input)
      val result =
        underTest.findZValues(assignments, instructions) shouldBe List(0, 0, 1,
          1, 1, 1, 1, 1, 0, 1, 0, 0, 0)
    }

    "part 1" in {
      val (assignments, instructions) = parse(input)
      val result = underTest.part1(assignments, instructions) shouldBe 2024
    }
  }

  "solve" should {
    val input =
      """x00: 1
        |x01: 1
        |x02: 0
        |x03: 0
        |x04: 0
        |x05: 1
        |x06: 0
        |x07: 1
        |x08: 1
        |x09: 0
        |x10: 1
        |x11: 0
        |x12: 0
        |x13: 0
        |x14: 1
        |x15: 0
        |x16: 0
        |x17: 0
        |x18: 1
        |x19: 0
        |x20: 0
        |x21: 1
        |x22: 0
        |x23: 0
        |x24: 0
        |x25: 1
        |x26: 1
        |x27: 1
        |x28: 0
        |x29: 1
        |x30: 1
        |x31: 1
        |x32: 0
        |x33: 0
        |x34: 0
        |x35: 1
        |x36: 0
        |x37: 1
        |x38: 0
        |x39: 0
        |x40: 0
        |x41: 0
        |x42: 0
        |x43: 0
        |x44: 1
        |y00: 1
        |y01: 0
        |y02: 1
        |y03: 1
        |y04: 0
        |y05: 0
        |y06: 1
        |y07: 1
        |y08: 0
        |y09: 1
        |y10: 1
        |y11: 1
        |y12: 1
        |y13: 0
        |y14: 1
        |y15: 1
        |y16: 0
        |y17: 1
        |y18: 0
        |y19: 1
        |y20: 0
        |y21: 0
        |y22: 0
        |y23: 1
        |y24: 0
        |y25: 0
        |y26: 0
        |y27: 0
        |y28: 1
        |y29: 0
        |y30: 0
        |y31: 0
        |y32: 1
        |y33: 1
        |y34: 0
        |y35: 0
        |y36: 0
        |y37: 1
        |y38: 1
        |y39: 0
        |y40: 1
        |y41: 0
        |y42: 0
        |y43: 0
        |y44: 1
        |
        |mcb OR tbc -> hkk
        |y15 XOR x15 -> fns
        |y02 AND x02 -> jtv
        |qcs XOR hrs -> z19
        |nvv XOR kvd -> z09
        |x33 AND y33 -> nqt
        |hcg AND cth -> kqt
        |y06 AND x06 -> ksg
        |mqk OR vbs -> mvs
        |jkq XOR tsw -> z38
        |y01 AND x01 -> gmn
        |rgg OR qgf -> vjq
        |jkq AND tsw -> dch
        |bbs XOR cbd -> z32
        |y22 XOR x22 -> dtf
        |x36 XOR y36 -> grd
        |pcj XOR svn -> z04
        |y06 XOR x06 -> tdv
        |jtn OR qcj -> tgw
        |gtv XOR mpd -> z35
        |x25 XOR y25 -> wjb
        |mpg OR jtv -> jmc
        |rnt AND kdv -> vsk
        |cth XOR hcg -> z43
        |x03 AND y03 -> jtm
        |dsh OR tcp -> bjc
        |kfd OR dkd -> fhf
        |y19 XOR x19 -> qcs
        |y22 AND x22 -> tcp
        |y28 AND x28 -> dpw
        |fdg XOR pnm -> z26
        |x17 AND y17 -> qbc
        |ngv OR rvn -> ptc
        |dpw OR ddn -> nct
        |x26 XOR y26 -> pnm
        |qdj AND jmc -> gdr
        |jnw XOR qss -> z37
        |x42 XOR y42 -> qwn
        |wwj XOR rsh -> z16
        |y39 XOR x39 -> hdv
        |qmn XOR fvt -> z24
        |tvn XOR jvq -> z07
        |y38 AND x38 -> qwg
        |qbw XOR fns -> z15
        |mkk OR mfk -> cbv
        |y34 AND x34 -> z34
        |x44 AND y44 -> bvb
        |y13 XOR x13 -> whd
        |gdj OR vsk -> wbv
        |x09 AND y09 -> mqk
        |x00 XOR y00 -> z00
        |hdv XOR pfr -> z39
        |jvq AND tvn -> dkd
        |ndm AND tsp -> vjh
        |btf XOR fhf -> z08
        |dtf AND wbv -> dsh
        |pfr AND hdv -> krw
        |wwb OR cpm -> jmt
        |wsg OR nqt -> mdh
        |rsk OR gmn -> vbb
        |cvp XOR fqv -> z25
        |dqc OR qbc -> vqr
        |y32 XOR x32 -> bbs
        |x39 AND y39 -> kwg
        |jvj XOR mvs -> mkk
        |cbv XOR bmt -> z11
        |bbk OR ptt -> pgg
        |pjm AND rvg -> ddn
        |grd XOR wgp -> z36
        |jnw AND qss -> mvk
        |x04 AND y04 -> pmq
        |ncs OR qcb -> cth
        |sgv AND ntn -> rsk
        |y15 AND x15 -> knd
        |hgk XOR kvk -> z20
        |y07 XOR x07 -> jvq
        |x27 XOR y27 -> fjb
        |vjq AND tmg -> kbk
        |mvs AND jvj -> z10
        |x18 AND y18 -> rjw
        |y11 AND x11 -> ngv
        |x07 AND y07 -> kfd
        |tfg XOR vrb -> z44
        |y24 XOR x24 -> qmn
        |mdh AND jmq -> cgh
        |pcj AND svn -> nkq
        |x20 XOR y20 -> kvk
        |x12 XOR y12 -> hnk
        |y44 XOR x44 -> tfg
        |wwj AND rsh -> qcj
        |x33 XOR y33 -> fms
        |y01 XOR x01 -> ntn
        |y32 AND x32 -> wwb
        |jmt XOR fms -> z33
        |kdv XOR rnt -> z21
        |bvj OR cbg -> kbc
        |y29 AND x29 -> rgg
        |y05 XOR x05 -> sgb
        |qmk AND rdb -> rmw
        |cgh OR wcb -> mpd
        |x12 AND y12 -> bvj
        |kbc XOR whd -> z13
        |rmw OR hmm -> tjt
        |skc OR knd -> wwj
        |fgc OR rgp -> fvt
        |x41 AND y41 -> bqd
        |x13 AND y13 -> sbf
        |qcn OR wjb -> fdg
        |qrt AND vqr -> tvq
        |x23 XOR y23 -> bfq
        |x24 AND y24 -> jfm
        |jmq XOR mdh -> wcb
        |y23 AND x23 -> rgp
        |tmg XOR vjq -> z30
        |x17 XOR y17 -> rps
        |y30 AND x30 -> dfh
        |y27 AND x27 -> ccs
        |fjb AND hkk -> gbc
        |x36 AND y36 -> qfv
        |bmt AND cbv -> rvn
        |x19 AND y19 -> bfp
        |tgw AND rps -> dqc
        |vqr XOR qrt -> z18
        |y10 XOR x10 -> jvj
        |gcd XOR vbb -> z02
        |pjm XOR rvg -> z28
        |y11 XOR x11 -> bmt
        |wkt AND pfh -> kqn
        |gbc OR ccs -> pjm
        |x14 XOR y14 -> tsp
        |qbw AND fns -> skc
        |x10 AND y10 -> mfk
        |cbd AND bbs -> cpm
        |sgb AND wqq -> bbk
        |bfq AND bjc -> fgc
        |sgv XOR ntn -> z01
        |y16 XOR x16 -> rsh
        |y30 XOR x30 -> tmg
        |kqt OR qws -> vrb
        |y41 XOR x41 -> pph
        |ndm XOR tsp -> qbw
        |x38 XOR y38 -> tsw
        |x14 AND y14 -> fhq
        |nvv AND kvd -> vbs
        |dkk OR tqj -> nvv
        |x08 XOR y08 -> btf
        |y35 AND x35 -> fgd
        |jpg XOR nct -> z29
        |x02 XOR y02 -> gcd
        |bfp OR hws -> hgk
        |jpg AND nct -> qgf
        |qwn AND hnj -> ncs
        |hjt OR sbf -> ndm
        |y28 XOR x28 -> rvg
        |x31 AND y31 -> gvk
        |x29 XOR y29 -> jpg
        |x25 AND y25 -> cvp
        |gtv AND mpd -> tmm
        |tmm OR fgd -> wgp
        |bjc XOR bfq -> z23
        |x21 XOR y21 -> rnt
        |x00 AND y00 -> sgv
        |wgp AND grd -> rrc
        |pfh XOR wkt -> z31
        |pkq OR ksg -> tvn
        |y08 AND x08 -> tqj
        |y34 XOR x34 -> jmq
        |rvw OR tcc -> kdv
        |x26 AND y26 -> tbc
        |x05 AND y05 -> ptt
        |x43 XOR y43 -> hcg
        |y03 XOR x03 -> qdj
        |jtm OR gdr -> svn
        |kbk OR dfh -> wkt
        |tdv AND pgg -> pkq
        |y09 XOR x09 -> kvd
        |pph XOR tjt -> z41
        |rjw OR tvq -> hrs
        |wbv XOR dtf -> z22
        |krw OR kwg -> qmk
        |vbb AND gcd -> mpg
        |y42 AND x42 -> qcb
        |jfm OR mcr -> fqv
        |hgk AND kvk -> tcc
        |x37 AND y37 -> mrv
        |y20 AND x20 -> rvw
        |wqq XOR sgb -> z05
        |y43 AND x43 -> qws
        |tgw XOR rps -> z17
        |y04 XOR x04 -> pcj
        |hnk AND ptc -> cbg
        |pnm AND fdg -> mcb
        |fms AND jmt -> wsg
        |x35 XOR y35 -> gtv
        |qmk XOR rdb -> z40
        |x16 AND y16 -> jtn
        |vjh OR fhq -> z14
        |y21 AND x21 -> gdj
        |x18 XOR y18 -> qrt
        |vrb AND tfg -> bwp
        |kqn OR gvk -> cbd
        |ptc XOR hnk -> z12
        |jcb OR bqd -> hnj
        |qmn AND fvt -> mcr
        |whd AND kbc -> hjt
        |pmq OR nkq -> wqq
        |cvp AND fqv -> qcn
        |fhf AND btf -> dkk
        |x40 XOR y40 -> rdb
        |pgg XOR tdv -> z06
        |mvk OR mrv -> jkq
        |hkk XOR fjb -> z27
        |hrs AND qcs -> hws
        |bwp OR bvb -> z45
        |qwn XOR hnj -> z42
        |x31 XOR y31 -> pfh
        |x40 AND y40 -> hmm
        |jmc XOR qdj -> z03
        |x37 XOR y37 -> qss
        |tjt AND pph -> jcb
        |rrc OR qfv -> jnw
        |qwg OR dch -> pfr""".stripMargin
    "part 1" in {
      val (assignments, instructions) = parse(input)
      val result =
        underTest.part1(assignments, instructions) shouldBe 36902370467952L
    }

    // z00 = x00 XOR y00
    // carry00 = x00 AND y00
    // z01 = x01 XOR y01 XOR carry00 =
    // carry01 = (x01 AND y01) OR (x01 AND carry00) OR (y01 AND carry00)
    "print all" in {
      val (assignments, instructions) = parse(input)
      println("--------------------")
      val instructionsMap: mutable.Map[String, Instruction] =
        mutable.Map.empty[String, Instruction]
      instructions.foreach { instruction =>
        instructionsMap += instruction._4 -> Instruction(
          instruction._1,
          instruction._2,
          instruction._3
        )
      }
      underTest.printAll(instructionsMap)
    }

    "create add" in {
      val correctInstructions = underTest.createAddInstructions()
      println("--------------------")
      for ((k, v) <- correctInstructions) {
        println(s"$k = ${v.input1} ${v.operation} ${v.input2}")
      }

      val (assignments, instructions) = parse(input)
      println("--------------------")
      val instructionsMap: mutable.Map[String, Instruction] =
        mutable.Map.empty[String, Instruction]
      instructions.foreach { instruction =>
        instructionsMap += instruction._4 -> Instruction(
          instruction._1,
          instruction._2,
          instruction._3
        )
      }
      correctInstructions.size shouldBe instructionsMap.size
    }

    "replaces" in {
      val (assignments, instructions) = parse(input)
      println("--------------------")
      val instructionsMap: mutable.Map[String, Instruction] =
        mutable.Map.empty[String, Instruction]
      instructions.foreach { instruction =>
        instructionsMap += instruction._4 -> Instruction(
          instruction._1,
          instruction._2,
          instruction._3
        )
      }

      underTest.part2(instructionsMap)
    }
  }
}