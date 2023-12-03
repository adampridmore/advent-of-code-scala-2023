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
  
  def solveA(textInput : String ) : Int = {
    val lines = textInput.split(System.lineSeparator());

    def processLine(line: String) : Int = {
      var digitCharacters = 
          line.toCharArray()
          .filter(Character.isDigit(_))

      Integer.parseInt(digitCharacters.head.toString() + digitCharacters.last.toString())
    }

    var lineValues = lines
      .filter(!_.isBlank())
      .map(processLine(_))

    lineValues.sum
  }
  
  "Day 1" should {
    "Part 1 Example" in {
      assertAndPrint("Part 1 example", solveA(exampleText), 142)
    }

    "Part 1 puzzle" in {
      assertAndPrint("Part 1 puzzle", solveA(puzzleText()), 54338)
    }
  }
}
