package parser

import parser.CharCombinator._
import parser.ParserCombinator.runParser

class CharCombinatorSpec extends TestBase("CharCombinator") {
  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')
  val parserC: Parser[Char] = parseChar('C')
  val parserH: Parser[Char] = parseChar('H')

  it should "return a Parser object for parserChar" in {
    parserA shouldBe an[Parser[_]]
  }

  it should
    "return remaining input & the matched character if the char is matched" in {
    runParser("ABC", parserA) shouldBe Right('A', "BC")
  }

  it should "return a failure msg in case of no match" in {
    runParser("ABC", parserC) shouldBe Left("Error parsing 'C'. Unexpected 'A'")
  }

  it should "return No more input for empty and null string inputs" in {
    runParser("", parserC).merge shouldBe "No more input"
    runParser("", parserC).merge shouldBe "No more input"
  }

  it should "run the parser on the given input" in {
    runParser("Hello", parserH).merge shouldBe ('H', "ello")
    runParser("hola", parserH).merge shouldBe "Error parsing 'H'. Unexpected 'h'"
    runParser("", parserH).merge shouldBe "No more input"
  }

  it should "build a parser to match any char with 'anyOf'" in {
    val chars = ('a' to 'z').toList
    val anyCharParser = anyCharOf(chars)
    runParser("abcd", anyCharParser) shouldBe Right(('a', "bcd"))
  }

  it should "parse lowercase characters" in {
    runParser("abc", parseLowerCase) shouldBe Right(('a', "bc"))
    runParser("zxg", parseLowerCase) shouldBe Right(('z', "xg"))
  }

  it should "parse digits" in {
    runParser("123", parseDigit) shouldBe Right(('1', "23"))
    runParser("323", parseDigit) shouldBe Right(('3', "23"))
  }

  it should "parse given string as list of chars in the input with allOf" in {
    val stringToMatch = "Naruto".toList
    val input = "Naruto Uzumaki"

    val parser = allCharsOf(stringToMatch)
    val expected = Right((List('N', 'a', 'r', 'u', 't', 'o'), " Uzumaki"))

    runParser(input, parser) shouldBe expected
  }

  it should "get a List of elements when parsed by one or more digit" in {
    val comma = parseChar(',')
    val digit = parseDigit
    val pList = parseOneOrMoreDigitAsList(digit, comma)

    runParser("1,2,3,4", pList) shouldBe Right((List(1, 2, 3, 4), ""))
    runParser("1", pList) shouldBe Right((List(1), ""))
  }

    it should "return a helpful message with label set" in {
      parseChar('c') <-> "alphabet"
      val parseDigitWithLabel: Parser[Char] = parseDigit <-> "digit"
      runParser("A", parseDigitWithLabel) shouldBe Left(
        "Error parsing 'digit'. Unexpected 'A'")
    }

}
