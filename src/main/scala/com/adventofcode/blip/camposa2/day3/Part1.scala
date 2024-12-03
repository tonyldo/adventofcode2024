package com.adventofcode.blip.camposa2.day3

import com.adventofcode.blip.common.util.io.FileReader

import scala.jdk.CollectionConverters.*
import java.util.regex.Pattern
import scala.annotation.tailrec

object Part1 extends App with FileReader {
  def getMutiplyTuples(line: String): List[(Int,Int)] = {
    val regex = """mul\([0-9]{1,3},[0-9]{1,3}\)"""
    val matches = Pattern.compile(regex).matcher(line)
    val result = matches
      .results().
      map(_.group()).
      toList.
      asScala.
      map(m=>Pattern.
        compile("""[0-9]{1,3},[0-9]{1,3}""").
        matcher(m).
        results().
        map(_.group()).
        map(_.toString).
        toList.
        get(0)).
      map(_.split(",")).
      map(x => (x(0).toInt, x(1).toInt)).
      toList

    result
  }

  @tailrec
  private def cleanTheLine(line:StringBuilder): StringBuilder ={
    val indexOfDont = line.indexOf("""don't()""")
    if (indexOfDont == -1) {
      line
    } else {
      val indexOfDo = line.indexOf("""do()""")
      if indexOfDo != -1 then
        if (indexOfDo < indexOfDont)
          cleanTheLine(line.replace(indexOfDo, indexOfDo + 4, ""))
        else
          cleanTheLine(line.replace(indexOfDont, indexOfDo + 4, ""))
      else
        cleanTheLine(line.replace(indexOfDont, line.size, ""))
    }
  }

  val stream = readLinesFromFile("src/main/resources/input_puzzle_day_3.txt")
  val tuples = stream.map(getMutiplyTuples).flatMap(_.toList)
  println(tuples.map(x => x._1 * x._2).sum)



  //println(cleanTheLine(StringBuilder("""xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)un""")))

  val stream2 = readLinesFromFile("src/main/resources/input_puzzle_day_3.txt").mkString("")

  val tuples2 = List(stream2).map(s=>cleanTheLine(StringBuilder(s)).toString()).
      map(getMutiplyTuples).flatMap(_.toList)
    println(tuples2.map(x => x._1 * x._2).sum)
}
