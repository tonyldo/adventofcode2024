package com.adventofcode.blip.camposa2.day1

import com.adventofcode.blip.common.util.io.FileReader

import scala.annotation.tailrec

object Part1 extends App {
  private case class CalculateDistanceFromFile(fileName: String) extends FileReader {
    def calculateDistance(): Int = {
      @tailrec
      def calculateDistance(idTuples: List[(Int,Int)], distanceSum: Int): Int = {
        if (idTuples.isEmpty) {
          distanceSum
        } else {
          calculateDistance(idTuples.tail, distanceSum + math.abs(idTuples.head._1 - idTuples.head._2))
        }
      }
      val idTuplesOutOfOrder = readLinesFromFile(fileName).
        map(_.split("\\s+")).
        map(x => (x(0).toInt, x(1).toInt)).
        unzip
      val idTuples = idTuplesOutOfOrder._1.sorted.zip(idTuplesOutOfOrder._2.sorted)
      calculateDistance(idTuples, 0)
    }
  }
  private val result = CalculateDistanceFromFile("src/main/resources/input_puzzle_day_1.txt").calculateDistance()
  println(result)
}
