package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper {
  def inputFilename() = "day1/input.txt"

  private def textToInts(lines: String) : Seq[Option[Int]] = {
    lines
      .split(System.lineSeparator())
      .map(line => line.trim)
      .map({
          case "" => None
          case line => Some(line.toInt)
        })
      .toList
  }

  val dataInts : Seq[Option[Int]] = textToInts(data)

  val exampleText : String = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

  val exampleInts : Seq[Option[Int]] = textToInts(exampleText)

  def assertAndPrint[T](message: String, answer: T, expected: T) = {
    answer shouldBe expected
  }
  
  def splitIntoElves(textInput : String ) : Seq[Int] = {
    
    val lines = textToInts(textInput)

    val elves = Array.newBuilder[Int]
    
    var caloriesTotal = 0
    for (line <- lines){
      line match {
        case None => {
          elves += caloriesTotal
          caloriesTotal = 0
        }
        case Some(calories) => caloriesTotal += calories
      }
    }
    elves += caloriesTotal

    elves.result()
  }

  def solveA(textInput : String ) : Int = {
    val elves = splitIntoElves(textInput)
    elves.max
  }

  def solveB(textInput : String ) : Int = {
    splitIntoElves(textInput)
      .sortBy(identity)
      .reverse
      .take(3)
      .sum
  }
  
  "Day 1" should {
    "Part 1 Example" in {
      assertAndPrint("Part 1 example", solveA(exampleText), 24000)
    }

    "Part 1" in {
       assertAndPrint("Part 1", solveA(data), 69795)
     }

    "Part 2 Example" in {
      assertAndPrint("Part 1 example", solveB(exampleText), 45000)
     }

    "Part 2" in {
      assertAndPrint("Part 1", solveB(data), 208437)
     }
  }
}
