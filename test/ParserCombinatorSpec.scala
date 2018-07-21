import ParserCombinator._

class ParserCombinatorSpec extends TestBase("ParseCombinator") {

  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')
  val parserC: Parser[Char] = parseChar('C')
  val parserH: Parser[Char] = parseChar('H')

  val parserList = List(parserA, parserB, parserC, parserH)

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
    val combinedParser = choice(parserList)
    runParser("ABC", combinedParser) shouldBe Right(('A', "BC"))
    runParser("BC", combinedParser) shouldBe Right(('B', "C"))
  }

  it should "build a parser to match any char with 'anyOf'" in {
    val chars = ('a' to 'z').toList
    val anyCharParser = anyOf(chars)
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

  it should "parse input sequentially with the given parsers" in {
    val sequentialParser = sequence(parserList)
    val expected = Right((List('A', 'B', 'C', 'H'), "XYZ"))
    runParser("ABCHXYZ", sequentialParser) shouldBe expected
  }

  it should "parse given string as list of chars in the input with allOf" in {
    val stringToMatch = "Naruto".toList
    val input = "Naruto Uzumaki"

    val parser = ParserCombinator.allOf(stringToMatch)
    val expected = Right((List('N', 'a', 'r', 'u', 't', 'o'), " Uzumaki"))

    runParser(input, parser) shouldBe expected
  }

  it should "parse the given string in the input and return a string" in {
    val parser1 = parseString("Boruto")
    val parser2 = parseString("ABC")

    runParser("Boruto Uzumaki", parser1) shouldBe Right(("Boruto", " Uzumaki"))
    runParser("ABC|DE", parser2) shouldBe Right(("ABC", "|DE"))
  }

  it should "parse any number of A's in the input" in {
    parseZeroOrMore(parserA, "AAABCD") shouldBe (List('A', 'A', 'A'), "BCD")
    parseZeroOrMore(parserA, "ABCD") shouldBe (List('A'), "BCD")
    parseZeroOrMore(parserA, "BCD") shouldBe (List(), "BCD")
    parseZeroOrMore(parserB, "AAABCD") shouldBe (List(), "AAABCD")
  }

  it should "parse any number of A's in the input with many" in {
    val manyA = **(parserA)
    runParser("AABCD", manyA) shouldBe Right("AA", "BCD")
    runParser("BCD", manyA) shouldBe Right("", "BCD")
  }

  it should "parse One or more chars in the input" in {
    val atleastOneA = ++(parserA)
    runParser("AAABCD", atleastOneA) shouldBe Right(("AAA", "BCD"))
    runParser("ABCD", atleastOneA) shouldBe Right(("A", "BCD"))
    runParser("BCD", atleastOneA) shouldBe Left("Expecting 'A'. Got 'B'")
  }

  it should "parse a character if it is present or not with opt" in {
    val optionalA = ??(parserA)
    runParser("ABC", optionalA) shouldBe Right(("A", "BC"))
    runParser("BC", optionalA) shouldBe Right(("", "BC"))
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
