package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper {
  def inputFilename() = "day1/input.txt"

  def puzzleText() : String = {
    data
  }

  val exampleText : String = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""

  def assertAndPrint[T](message: String, answer: T, expected: T) = {
    println(s"Puzzle: $message: Answer: $answer")
    answer shouldBe expected
  }

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

  def processLinePartA : String => Int = processLine(numbers)
  def processLinePartB : String => Int = processLine(numbersAndWords)

  def solveA(textInput : String, lineProcessor: (String => Int)) : Int = {
    val lines = textInput.split(System.lineSeparator());

    var lineValues = lines
      .filter(!_.isBlank())
      .map(lineProcessor(_));

    lineValues.sum;
  }
  
  "Day 1" should {
    "Part 1" should {
      "Example" in {
        assertAndPrint("Part 1 example", solveA(exampleText,processLinePartA), 142)
      }

      "parse line" should {
        var line1 = "1abc2"
        var line2 = "pqr3stu8vwx"
        var line3 = "a1b2c3d4e5f"
        var line4 = "treb7uchet"
        
        line1 + " process to 12" in {
          processLinePartA(line1) shouldBe 12
        }
        line2 + " process to 38" in {
          processLinePartA(line2) shouldBe 38
        }
        line3 + " process to 15" in {
          processLinePartA(line3) shouldBe 15
        }
        line4 + " process to 77" in {
          processLinePartA(line4) shouldBe 77
        }
      }

      "puzzle" in {
        assertAndPrint("Part 1 puzzle", solveA(puzzleText(), processLinePartA), 54338)
      }
    }

    "Part 2" should {

      "Part 2 parse line" should {
        var line1 = "two1nine"
        var line2 = "eightwothree"
        var line3 = "abcone2threexyz"
        var line4 = "xtwone3four"
        var line5 = "4nineeightseven2"
        var line6 = "zoneight234"
        var line7 = "7pqrstsixteen"
               
        line1 + " process to 29" in {
          firstNumber(line1, numbersAndWords) shouldBe 2
          lastNumber(line1, numbersAndWords) shouldBe 9
        }

        line2 + "process to 83" in {
          processLinePartB(line2) shouldBe 83
        }

        line3 + "process to 22" in {
          firstNumber(line3, numbersAndWords) shouldBe 1
          lastNumber(line3, numbersAndWords) shouldBe 3

          processLinePartB(line3) shouldBe 13
        }

        line4 + "process to 24" in {
          processLinePartB(line4) shouldBe 24
        }

        line5 + "process to 42" in {
          processLinePartB(line5) shouldBe 42
        }

        line6 + "process to 14" in {
          processLinePartB(line6) shouldBe 14
        }

        line7 + "process to 76" in {
          processLinePartB(line7) shouldBe 76
        }

        "temp 1" in {
          firstNumber("pqr3stu8vwx", numbersAndWords) shouldBe 3
          lastNumber("pqr3stu8vwx", numbersAndWords) shouldBe 8
        }

        "temp 2" in {
          firstNumber("7pqrstsixteen", numbersAndWords) shouldBe 7
          lastNumber("7pqrstsixteen", numbersAndWords) shouldBe 6
        }
      }

      "puzzle" in {
        assertAndPrint("Part 2 puzzle", solveA(puzzleText(), processLinePartB), 53389)
      }
    }
  }
}
