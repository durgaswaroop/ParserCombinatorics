package parser

import parser.CharCombinator._
import parser.ParserCombinator._

class ParserSpec extends TestBase("Parser") {
  val parserA = parseChar('A')
  val parserB = parseChar('B')
  val parserC = parseChar('C')

  val parser1 = parseChar('1')
  val parser2 = parseChar('2')

  it should "parse correctly when I combine orElse with andThen" in {
    val aAndThenBorC = parserA >> (parserB <|> parserC)

    runParser("ABZ", aAndThenBorC) shouldBe Right(('A', 'B'), "Z")
    runParser("ACZ", aAndThenBorC) shouldBe Right(('A', 'C'), "Z")
  }

  it should "parse correctly when I combine orElse with map" in {
    val oneOrElseTwo = parser1 <|> parser2
    val asInt = (c: Char) => c.asDigit
    val aOrElseBAsInt = oneOrElseTwo |>> asInt

    runParser("123", aOrElseBAsInt) shouldBe Right(1, "23")
  }

  it should "parse correctly when I combine andThen with map" in {
    //noinspection ScalaUnnecessaryParentheses
    val asString: ((Char, Char)) => String = t => t._1.toString + t._2.toString
    val aThenBAsString = parserA >> parserB |>> asString

    runParser("ABC", aThenBAsString) shouldBe Right("AB", "C")

    val oneThenTwoAsString = parser1 >> parser2 |>> asString

    runParser("12C", oneThenTwoAsString) shouldBe Right("12", "C")

    val asInt: String => Int = s => s.toInt
    val oneThenTwoAsStringAsInt = parser1 >> parser2 |>> asString |>> asInt

    runParser("12C", oneThenTwoAsStringAsInt) shouldBe Right(12, "C")
  }
}
