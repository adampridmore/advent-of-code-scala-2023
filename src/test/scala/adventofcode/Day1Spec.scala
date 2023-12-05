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
  
  "A Day1 solver 1" can {
    "solve part 1" should {
      "solve to 142" in {
        val day1 = new adventofcode.day1.Day1(exampleText)

        assertAndPrint("Part 1 example", day1.solve(Day1.processLinePartA), 142)
      }

      "parse line" should {
        var line1 = "1abc2"
        // var line1_ = ("1abc2", 12)
        var line2 = "pqr3stu8vwx"
        var line3 = "a1b2c3d4e5f"
        var line4 = "treb7uchet"

        val lines = List(
          ("1abc2", 12),
          ("pqr3stu8vwx",38),
          ("a1b2c3d4e5f", 15),
          ("treb7uchet", 77)
        )
        
        "pass all lines" in {
          lines.foreach(line => {
            Day1.processLinePartA(line._1) shouldBe line._2
          })
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

        var lines = List(
          ("two1nine", "29"),
          ("eightwothree","83"),
          ("abcone2threexyz","13"),
          ("xtwone3four","24"),
          ("4nineeightseven2","42"),
          ("zoneight234","14"),
          ("7pqrstsixteen","76")
        )

        "test lines" in{
          lines.foreach(line => {
            
            Day1.firstNumber(line._1, Day1.numbersAndWords) shouldBe (Integer.parseInt(line._2.charAt(0).toString()))
            Day1.lastNumber(line._1, Day1.numbersAndWords) shouldBe (Integer.parseInt(line._2.charAt(1).toString()))
            Day1.processLinePartB(line._1) shouldBe (Integer.parseInt(line._2))
          })
        }
               
        "bug 1" in {
          Day1.firstNumber("pqr3stu8vwx", Day1.numbersAndWords) shouldBe 3
          Day1.lastNumber("pqr3stu8vwx", Day1.numbersAndWords) shouldBe 8
        }

        "bug 2" in {
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
