package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import adventofcode.day1.Day1
import adventofcode.AdventOfCodeFileHelper

class Day1Spec extends AnyWordSpec with Matchers with AdventOfCodeFileHelper {
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
  
  "Day 1" should {
    "Part 1" should {
      "Example" in {
        val day1 = new adventofcode.day1.Day1(exampleText)

        assertAndPrint("Part 1 example", day1.solve(Day1.processLinePartA), 142)
      }

      "parse line" should {
        var line1 = "1abc2"
        var line2 = "pqr3stu8vwx"
        var line3 = "a1b2c3d4e5f"
        var line4 = "treb7uchet"
        
        line1 + " process to 12" in {
          Day1.processLinePartA(line1) shouldBe 12
        }
        line2 + " process to 38" in {
          Day1.processLinePartA(line2) shouldBe 38
        }
        line3 + " process to 15" in {
          Day1.processLinePartA(line3) shouldBe 15
        }
        line4 + " process to 77" in {
          Day1.processLinePartA(line4) shouldBe 77
        }
      }

      "puzzle" in {
        val day1 = new Day1(puzzleText())
        assertAndPrint("Part 1 puzzle",  day1.solve(Day1.processLinePartA), 54338)
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
          Day1.firstNumber(line1, Day1.numbersAndWords) shouldBe 2
          Day1.lastNumber(line1, Day1.numbersAndWords) shouldBe 9
        }

        line2 + "process to 83" in {
          Day1.processLinePartB(line2) shouldBe 83
        }

        line3 + "process to 22" in {
          Day1.firstNumber(line3, Day1.numbersAndWords) shouldBe 1
          Day1.lastNumber(line3, Day1.numbersAndWords) shouldBe 3

          Day1.processLinePartB(line3) shouldBe 13
        }

        line4 + "process to 24" in {
          Day1.processLinePartB(line4) shouldBe 24
        }

        line5 + "process to 42" in {
          Day1.processLinePartB(line5) shouldBe 42
        }

        line6 + "process to 14" in {
          Day1.processLinePartB(line6) shouldBe 14
        }

        line7 + "process to 76" in {
          Day1.processLinePartB(line7) shouldBe 76
        }

        "temp 1" in {
          Day1.firstNumber("pqr3stu8vwx", Day1.numbersAndWords) shouldBe 3
          Day1.lastNumber("pqr3stu8vwx", Day1.numbersAndWords) shouldBe 8
        }

        "temp 2" in {
          Day1.firstNumber("7pqrstsixteen", Day1.numbersAndWords) shouldBe 7
          Day1.lastNumber("7pqrstsixteen", Day1.numbersAndWords) shouldBe 6
        }
      }

      "puzzle" in {
        val day1 = Day1(puzzleText())
        assertAndPrint("Part 2 puzzle", day1.solve(Day1.processLinePartB), 53389)
      }
    }
  }
}
