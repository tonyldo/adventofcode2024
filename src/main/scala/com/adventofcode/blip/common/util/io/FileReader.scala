package com.adventofcode.blip.common.util.io

trait FileReader {
  def readLinesFromFile(fileName: String): List[String] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines().toList
    source.close()
    lines
  }
}
