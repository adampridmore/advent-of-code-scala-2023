package adventofcode

import scala.io.Source

trait AdventOfCodeHelper {

  def inputFilename() : String

  def data : String = allLines.mkString(System.lineSeparator())

  def getLines(text: String) : Seq[String] = text
    .split(System.lineSeparator())
    .filter(!_.isBlank)
    .map(_.trim)

  def dataLines = allLines
      .filter(!_.isBlank())
      .map(_.trim())
      .toSeq

  private def allLines = 
      Source.fromResource(inputFilename()).getLines()
}
