package parser

import parser.ParserCombinator.runParser
import parser.StringCombinator._

class StringCombinatorSpec extends TestBase("StringCombinator") {

  it should "parse the given string in the input and return a string" in {
    val parser1 = parseString("Boruto")
    val parser2 = parseString("ABC")

    runParser("Boruto Uzumaki", parser1) shouldBe Right(("Boruto", " Uzumaki"))
    runParser("ABC|DE", parser2) shouldBe Right(("ABC", "|DE"))
  }

  it should "parse whitespace in the input string" in {
    runParser("  abc", parseWhiteSpace) shouldBe Right(("  ", "abc"))
    runParser("abc", parseWhiteSpace) shouldBe Right(("", "abc"))
    runParser("    ", parseWhiteSpace) shouldBe Right(("    ", ""))
    runParser("\tabc", parseWhiteSpace) shouldBe Right(("\t", "abc"))
    runParser("\t  abc", parseWhiteSpace) shouldBe Right(("\t  ", "abc"))
    runParser("\nabc", parseWhiteSpace) shouldBe Right(("\n", "abc"))
  }

}
