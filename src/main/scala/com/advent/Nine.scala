package com.advent

class Nine {

  def toBlocks(diskMap: String): Array[Either[Char, Int]] = {
    var res: Array[Either[Char, Int]] = Array()
    var i: Int = 0
    while (2 * i < diskMap.length) {
      val blocksNumb: Int = diskMap(2 * i).asDigit
      for (j <- 0 until blocksNumb) {
        res = res :+ Right(i)
      }

      if (2 * i + 1 < diskMap.length) {
        val freeSpaceNumb: Int = diskMap(2 * i + 1).asDigit
        for (j <- 0 until freeSpaceNumb) {
          res = res :+ Left('.')
        }
      }
      i += 1
    }
    res
  }

  def compact(blocks: Array[Either[Char, Int]]): Array[Either[Char, Int]] = {
    var i = 0 // tracking index from the beginning
    var j = blocks.length - 1 // tracking index from the end
    while (i < blocks.length && j > i) {
      if (blocks(i).isLeft) { // is a dot
        if (blocks(j).isRight) { // is a digit
          val tmp = blocks(i)
          blocks(i) = blocks(j)
          blocks(j) = tmp
          // move both after swapping
          i += 1
          j -= 1
        } else {
          j -= 1 // move only j to find a digit
        }
      } else if (blocks(j).isRight) { // is a digit
        i += 1 // move only i to find a dot
      } else {
        // move both i is not a dot and j is not a digit
        i += 1
        j -= 1
      }
    }
    blocks
  }

  def part1(diskMap: String): BigInt = {
    val blocks = toBlocks(diskMap)
    val compacted = compact(blocks)
    compacted
      .flatMap(either => either.toOption)
      .zipWithIndex
      .map((id, index) => BigInt(id * index))
      .sum
  }

//  def toBlocks(diskMap: String): String = {
//    var res: String = ""
//    var i: Int = 0
//    while (2 * i < diskMap.length) {
//      val blocksNumb: Int = diskMap(2 * i).asDigit
//      res += i.toString * blocksNumb
//      if (2 * i + 1 < diskMap.length) {
//        val freeSpaceNumb: Int = diskMap(2 * i + 1).asDigit
//        res += "." * freeSpaceNumb
//      }
//      i += 1
//    }
//    res
//  }
//
//  def compact(blocks: String): String = {
//    var arr = blocks.toArray
//    var i = 0 // tracking index from the beginning
//    var j = arr.length - 1 // tracking index from the end
//    while (i < arr.length && j > i) {
//      if (arr(i) == '.') {
//        if (arr(j).isDigit) {
//          val tmp = arr(i)
//          arr(i) = arr(j)
//          arr(j) = tmp
//          // move both after swapping
//          i += 1
//          j -= 1
//        } else {
//          j -= 1 // move only j to find a digit
//        }
//      } else if (arr(j).isDigit) {
//        i += 1 // move only i to find a dot
//      } else {
//        // move both i is not a dot and j is not a digit
//        i += 1
//        j -= 1
//      }
//    }
//    arr.mkString
//  }
//
//  def part1(diskMap: String): BigInt = {
//    val blocks = toBlocks(diskMap)
//    //println(blocks)
//    val compacted = compact(blocks)
//    //println(compacted)
//    val onlyDigits = compacted
//      .substring(0, compacted.indexOf('.'))
//    val onlyDigitsWithIndex = onlyDigits.zipWithIndex
//    //println(onlyDigitsWithIndex)
//    onlyDigitsWithIndex
//      .map((char, index) => BigInt(char.asDigit * index))
//      .sum
//  }
}
