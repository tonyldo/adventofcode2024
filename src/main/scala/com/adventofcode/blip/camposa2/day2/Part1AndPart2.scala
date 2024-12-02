package com.adventofcode.blip.camposa2.day2

import com.adventofcode.blip.common.util.io.FileReader

import scala.annotation.tailrec

object Part1AndPart2 extends App with FileReader {
  private def isSafeReport(numbers: List[Int]): Boolean = {
    @tailrec
    def isSafeReport(numbers: List[Int],
                     testingTheOrder: (Int,Int) => Boolean,
                     testingTheDiffer: (Int,Int) => Boolean,
                     isSafe: Boolean): Boolean = {
      numbers match {
        case x :: Nil => isSafe
        case x :: y :: xs => isSafeReport(y::xs,
          testingTheOrder,
          testingTheDiffer,
          isSafe && testingTheOrder(x, y) && testingTheDiffer(x, y))
        case _ => isSafe
      }
    }

    val testingTheOrder: (Int,Int) => Boolean = numbers match
      case x :: y :: xs if x < y => (x: Int, y: Int) => x < y
      case x :: y :: xs if x > y => (x: Int, y: Int) => x > y
      case _ => (x: Int, y: Int) => x < y

    val testingTheDiffer: (Int,Int) => Boolean = (x: Int, y: Int) => {
      math.abs(x - y) > 0 && math.abs(x - y) <= 3
    }

    val result = isSafeReport(numbers, testingTheOrder, testingTheDiffer, true)
    println(s"$numbers - $result")
    result
  }

  private def removeOneLevelAndTest(numbers: List[Int]): Boolean = {
    def removeOneLevelAndTest(numbers: List[Int], index: Int): Boolean = {
      if (isSafeReport(numbers)){
        true
      } else if (numbers.length - index >= 1) {
        isSafeReport(numbers.patch(index,Nil,1)) || removeOneLevelAndTest(numbers, index + 1)
      } else {
        false
      }
    }

    removeOneLevelAndTest(numbers, 0)
  }


  private val reports = readLinesFromFile("src/main/resources/input_puzzle_day_2.txt").
    map(_.split("\\s+")).map(_.map(_.toInt)).map(_.toList)

  private val safes = reports.filter(isSafeReport)
  println(safes.length)

  private val safesWithoutOneLevel = reports.filter(removeOneLevelAndTest)
  println(safesWithoutOneLevel.length)
}