package com.adventofcode.blip.camposa2.day1

import com.adventofcode.blip.common.util.io.FileReader

import scala.annotation.tailrec

object Part2 extends App {
  private case class FindSimilarityFromFile(fileName: String) extends FileReader {
    def findSimilarity(): Int = {
      @tailrec
      def findSimilarity(idTuples: (List[Int],List[Int]), similaritySum: Int): Int = {
        if (idTuples._1.isEmpty) {
          similaritySum
        } else {
          findSimilarity((idTuples._1.tail,idTuples._2), similaritySum + (idTuples._1.head * idTuples._2.count(_ == idTuples._1.head)))
        }
      }
      val idTuplesOutOfOrder = readLinesFromFile(fileName).
        map(_.split("\\s+")).
        map(x => (x(0).toInt, x(1).toInt)).
        unzip
      findSimilarity(idTuplesOutOfOrder, 0)
    }
  }
  private val result = FindSimilarityFromFile("src/main/resources/input_puzzle_day_1.txt").findSimilarity()
  println(result)
}
