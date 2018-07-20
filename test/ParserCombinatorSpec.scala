import ParserCombinator._

class ParserCombinatorSpec extends TestBase("ParseCombinator") {

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
    runParser("ABC", parserC) shouldBe Left("Expecting 'C'. Got 'A'")
  }

  it should "return No more input for empty and null string inputs" in {
    runParser("", parserC).merge shouldBe "No more input"
    runParser("", parserC).merge shouldBe "No more input"
  }

  it should "run the parser on the given input" in {
    runParser("Hello", parserH).merge shouldBe ('H', "ello")
    runParser("hola", parserH).merge shouldBe "Expecting 'H'. Got 'h'"
    runParser("", parserH).merge shouldBe "No more input"
  }

  it should "combine all parsers with 'choice'" in {
    val combinedParser = choice(List(parserA, parserB, parserC))
    runParser("ABC", combinedParser) shouldBe Right('A', "BC")
    runParser("BC", combinedParser) shouldBe Right('B', "C")
  }

  it should "build a parser to match any char with 'anyOf'" in {
    val chars = ('a' to 'z').toList
    val anyCharParser = anyOf(chars)
    runParser("abcd", anyCharParser) shouldBe Right('a', "bcd")
  }

  it should "parse lowercase characters" in {
    runParser("abc", parseLowerCase) shouldBe Right('a', "bc")
    runParser("zxg", parseLowerCase) shouldBe Right('z', "xg")
  }

  it should "parse digits" in {
    runParser("123", parseDigit) shouldBe Right('1', "23")
    runParser("323", parseDigit) shouldBe Right('3', "23")
  }

}
