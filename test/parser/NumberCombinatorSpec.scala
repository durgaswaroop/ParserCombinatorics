package parser

import parser.NumberCombinator._
import parser.ParserCombinator._

class NumberCombinatorSpec extends TestBase("IntCombinator") {

  it should "parse a positive integer in the given input" in {
    runParser("1234", parseInt) shouldBe Right(1234, "")
    runParser("123.4", parseInt) shouldBe Left(
      "Error parsing '9'. Unexpected '.'")
  }

  it should "parse a negative integer in the given input" in {
    runParser("-1234", parseInt) shouldBe Right(-1234, "")
    runParser("-z1234", parseInt) shouldBe Left(
      "Error parsing '9'. Unexpected 'z'")
  }

  it should "parse a floating point number in the given input" in {
    runParser("123.456", parseFloat) shouldBe Right(123.456f, "")
  }

  it should "parse a negativefloating number in the given input" in {
    runParser("-123.45", parseFloat) shouldBe Right(-123.45f, "")
  }

}
