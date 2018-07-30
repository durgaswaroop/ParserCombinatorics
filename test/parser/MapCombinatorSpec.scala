package parser

import parser.CharCombinator._
import parser.ParserCombinator._

class MapCombinatorSpec extends TestBase("MapCombinator") {
  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')

  val isA: Char => Boolean = (x: Char) => x == 'A'
  val sameChar: Char => Char = (x: Char) => x
  val sameStr: String => String = (x: String) => x
  val toStr: Char => String = (x: Char) => x.toString
  val toInt: String => Int = (x: String) => x.toInt

  it should "transform parsed value with given function if parsing is successful" in {
    val mParserAF1 = parserA |>> isA
    runParser("ABC", mParserAF1) shouldBe Right((true, "BC"))

    val mParserAF2 = parserA |>> sameChar
    runParser("ABC", mParserAF2) shouldBe Right(('A', "BC"))

    val mParserBF3 = parserB |>> toStr
    runParser("BC", mParserBF3) shouldBe Right(("B", "C"))
  }

  it should "return if the parsing fails" in {
    val mParserBF1 = parserB |>> isA
    runParser("ABC", mParserBF1) shouldBe Left(
      "Error parsing 'B'. Unexpected 'A'")
    runParser("", mParserBF1) shouldBe Left("No more input")
  }

  it should "succeed for three functions in series" in {
    val mapped = parserA |>> sameChar |>> toStr |>> sameStr
    runParser("ABC", mapped) shouldBe Right(("A", "BC"))
  }
}
