package com.advent

case class DiskBlock(element: Either[Char, Int], index: Int, size: Int)
case class Disk(
    blocks: Array[Either[Char, Int]],
    fileBlocks: Array[DiskBlock],
    spaceBlocks: Array[DiskBlock]
)

class Nine {

  def toBlocks1(diskMapStr: String): Array[Either[Char, Int]] = {
    var res: Array[Either[Char, Int]] = Array()
    val diskMap: List[Int] = diskMapStr.toCharArray.map(_.asDigit).toList
    var i: Int = 0
    while (2 * i < diskMap.length) {
      val blocksNumb: Int = diskMap(2 * i)
      for (j <- 0 until blocksNumb) {
        res = res :+ Right(i)
      }

      if (2 * i + 1 < diskMap.length) {
        val freeSpaceNumb: Int = diskMap(2 * i + 1)
        for (j <- 0 until freeSpaceNumb) {
          res = res :+ Left('.')
        }
      }
      i += 1
    }
    res
  }

  def compact1(blocks: Array[Either[Char, Int]]): Array[Either[Char, Int]] = {
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

  def part1(diskMap: String): Long = {
    val blocks = toBlocks1(diskMap)
    val compacted = compact1(blocks)
    compacted.zipWithIndex
      .map((mayBeId, index) =>
        mayBeId match {
          case Right(id) => (id * index).toLong
          case _         => 0L
        }
      )
      .sum
  }

  val SpaceElement = Left('.')

  def toBlocks2(diskMapStr: String): Disk = {
    var res: Array[Either[Char, Int]] = Array()
    var files: Array[DiskBlock] = Array()
    var spaces: Array[DiskBlock] = Array()
    val diskMapList: List[Int] = diskMapStr.toCharArray.map(_.asDigit).toList
    val (oddIndexed, evenIndexed) = diskMapList.zipWithIndex.partition {
      case (_, index) =>
        index % 2 == 0 // even indexes
    }
    val filesList: List[Int] = oddIndexed.map(_._1)
    val spaceList: List[Int] = evenIndexed.map(_._1)
    var i: Int = 0
    while (i < filesList.length) {
      val filesNumb: Int = filesList(i)
      val fileElement = Right(i)
      files = files :+ DiskBlock(fileElement, res.length, filesNumb)
      for (j <- 0 until filesNumb) {
        res = res :+ fileElement
      }

      if (i < spaceList.length) {
        val spaceNumb: Int = spaceList(i)
        spaces = spaces :+ DiskBlock(SpaceElement, res.length, spaceNumb)
        for (j <- 0 until spaceNumb) {
          res = res :+ SpaceElement
        }
      }
      i += 1
    }
    Disk(res, files, spaces)
  }

  def compact2(disk: Disk): Array[Either[Char, Int]] = {
    for (i <- disk.fileBlocks.indices.reverse) {
      val fileBlock = disk.fileBlocks(i)
      val j = disk.spaceBlocks.indexWhere(spaceBlock =>
        spaceBlock.index < fileBlock.index && spaceBlock.size >= fileBlock.size
      )
      if (j >= 0) {
        // swap and reduce space
        val diskBlock = disk.spaceBlocks(j)
        for (k <- 0 until fileBlock.size) {
          val tmp = disk.blocks(fileBlock.index + k)
          disk.blocks(fileBlock.index + k) = disk.blocks(diskBlock.index + k)
          disk.blocks(diskBlock.index + k) = tmp
        }
        disk.spaceBlocks(j) = DiskBlock(
          SpaceElement,
          diskBlock.index + fileBlock.size,
          diskBlock.size - fileBlock.size
        )
      }
    }
    disk.blocks
  }

  def part2(diskMap: String): Long = {
    val disk = toBlocks2(diskMap)
    val compacted = compact2(disk)
    compacted.zipWithIndex
      .map((mayBeId, index) =>
        mayBeId match {
          case Right(id) => (id * index).toLong
          case _         => 0L
        }
      )
      .sum
  }
}
