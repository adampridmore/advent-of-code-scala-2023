package adventofcode.day1

import adventofcode.AdventOfCodeHelper

class Day1(textInput: String) extends AdventOfCodeHelper {

  def solve(lineProcessor: (String => Int)) : Int = {
    val lines = textInput.split(System.lineSeparator());

    var lineValues = lines
      .filter(!_.isBlank())
      .map(lineProcessor(_));

    lineValues.sum;
  }
}

object Day1{
  var numbers : List[(String, Int)] = 
    (1 to 10)
      .map(x => (x.toString, x))
      .toList
  
  val numbersAndWords = numbers ++ List(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  def firstNumber(line: String, numbersToMatch: List[(String, Int)]) : Int = {
    var maybeFoundNumber = numbersToMatch.find((text, x) => line.startsWith(text))
    maybeFoundNumber match {
      case Some(text, x) => x
      case None => firstNumber(line.substring(1), numbersToMatch)
    }
  }

  def lastNumber(line: String, numbersToMatch: List[(String, Int)]) : Int = {
    def reverNumbersToMatch(numbers: List[(String, Int)]) = numbers.map((text, x) => (text.reverse, x))
    
    firstNumber(line.reverse, reverNumbersToMatch(numbersToMatch))
  }

  def processLine(numberToMatch: List[(String, Int)])(line: String) : Int = {
    Integer.parseInt(firstNumber(line, numberToMatch).toString() + lastNumber(line, numberToMatch).toString())
  }

  def processLinePartA : String => Int = processLine(Day1.numbers)
  def processLinePartB : String => Int = processLine(Day1.numbersAndWords)
}
