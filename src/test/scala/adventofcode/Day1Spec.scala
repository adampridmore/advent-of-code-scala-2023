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

  val numbers = List( 
      "1" -> 1,
      "2" -> 2,
      "3" -> 3,
      "4" -> 4,
      "5" -> 5,
      "6" -> 6,
      "7" -> 7,
      "8" -> 8,
      "9" -> 9,

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

  def lastNumber(line: String) : Int = {
    def reverNumbersToMatch(numbers: List[(String, Int)]) = numbers.map((text, x) => (text.reverse, x))
    
    findNumber(line.reverse, reverNumbersToMatch(numbers))
  }

  def firstNumber(line: String) : Int = {
    findNumber(line, numbers)
  }

  def findNumber(line: String, numbers: List[(String, Int)]) : Int = {
    var maybeFoundNumber = numbers.find((text, x) => line.startsWith(text))
    maybeFoundNumber match {
      case Some(text, x) => x
      case None => findNumber(line.substring(1), numbers)
    }
  }

  def processLinePartA(line: String) : Int = {
    var digitCharacters = 
        line.toCharArray()
        .filter(Character.isDigit(_))

    Integer.parseInt(digitCharacters.head.toString() + digitCharacters.last.toString())
  }

  def processLinePartB(line: String) : Int = {
    Integer.parseInt(firstNumber(line).toString() + lastNumber(line).toString())
  }

  def solveA(textInput : String, lineProcessor: (String => Int)) : Int = {
    val lines = textInput.split(System.lineSeparator());

    var lineValues = lines
      .filter(!_.isBlank())
      .map(lineProcessor(_))

    lineValues.sum
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
        // var line1 = "two1nine"
        // var line2 = "eightwothree"
        // var line3 = "xtwone3four"
        // var line4 = "treb7uchet"

        var line1 = "two1nine"
        var line2 = "eightwothree"
        var line3 = "abcone2threexyz"
        var line4 = "xtwone3four"
        var line5 = "4nineeightseven2"
        var line6 = "zoneight234"
        var line7 = "7pqrstsixteen"
               
        line1 + " process to 29" in {
          firstNumber(line1) shouldBe 2
          lastNumber(line1) shouldBe 9
        }

        line2 + "process to 83" in {
          processLinePartB(line2) shouldBe 83
        }

        line3 + "process to 22" in {
          firstNumber(line3) shouldBe 1
          lastNumber(line3) shouldBe 3

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
          firstNumber("pqr3stu8vwx") shouldBe 3
          lastNumber("pqr3stu8vwx") shouldBe 8
        }

        "temp 2" in {
          // firstNumber("7pqrstsixteen") shouldBe 7
          lastNumber("7pqrstsixteen") shouldBe 6
        }
      }

      "puzzle" in {
        assertAndPrint("Part 2 puzzle", solveA(puzzleText(), processLinePartB), 54338)
      }

      // "temp" in {
      //   val lines = puzzleText().split(System.lineSeparator());

      //   var lineValues = lines
      //     .filter(!_.isBlank())
      //     .map(line => (line, processLinePartB(line)))
      //     // .mkString(System.lineSeparator())
      //     .foreach(println(_))
      // }
    }
  }
}
