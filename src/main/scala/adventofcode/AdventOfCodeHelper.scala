package adventofcode

import scala.io.Source

trait AdventOfCodeHelper {

  def getLines(text: String) : Seq[String] = text
    .split(System.lineSeparator())
    .filter(!_.isBlank)
    .map(_.trim) 
}

trait AdventOfCodeFileHelper extends AdventOfCodeHelper {
  def inputFilename() : String

  private def allLines = 
    Source.fromResource(inputFilename()).getLines()

  def data : String = allLines.mkString(System.lineSeparator())

  def dataLines = allLines
      .filter(!_.isBlank())
      .map(_.trim())
      .toSeq

}
