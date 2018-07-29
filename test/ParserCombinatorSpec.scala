import CharCombinator._
import ParserCombinator.{between => inBetween, _}
import StringCombinator._

class ParserCombinatorSpec extends TestBase("ParseCombinator") {

  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')
  val parserC: Parser[Char] = parseChar('C')
  val parserH: Parser[Char] = parseChar('H')

  val parserList = List(parserA, parserB, parserC, parserH)

  it should "combine all parsers with 'choice'" in {
    val combinedParser = choice(parserList)
    runParser("ABC", combinedParser) shouldBe Right(('A', "BC"))
    runParser("BC", combinedParser) shouldBe Right(('B', "C"))
  }

  it should "parse input sequentially with the given parsers" in {
    val sequentialParser = sequence(parserList)
    val expected = Right((List('A', 'B', 'C', 'H'), "XYZ"))
    runParser("ABCHXYZ", sequentialParser) shouldBe expected
  }

  it should "parse any number of A's in the input" in {
    parseZeroOrMore(parserA, "AAABCD") shouldBe (List('A', 'A', 'A'), "BCD")
    parseZeroOrMore(parserA, "ABCD") shouldBe (List('A'), "BCD")
    parseZeroOrMore(parserA, "BCD") shouldBe (List(), "BCD")
    parseZeroOrMore(parserB, "AAABCD") shouldBe (List(), "AAABCD")
  }

  it should "parse any number of A's in the input with many" in {
    val manyA = **(parserA)
    runParser("AABCD", manyA) shouldBe Right("AA".toList, "BCD")
    runParser("BCD", manyA) shouldBe Right("".toList, "BCD")
  }

  it should "parse One or more chars in the input" in {
    val atleastOneA = ++(parserA)
    runParser("AAABCD", atleastOneA) shouldBe Right(("AAA".toList, "BCD"))
    runParser("ABCD", atleastOneA) shouldBe Right(("A".toList, "BCD"))
    runParser("BCD", atleastOneA) shouldBe Left(
      "Error parsing 'A'. Unexpected 'B'")
  }

  it should "parse a character if it is present or not with opt" in {
    val optionalA = ??(parserA)
    runParser("ABC", optionalA) shouldBe Right(("A".toList, "BC"))
    runParser("BC", optionalA) shouldBe Right(("".toList, "BC"))
  }

  it should "match a parser but throw it away for >>!" in {
    val parseAButThrowAwayB = parserA >>! parserB
    runParser("ABC", parseAButThrowAwayB) shouldBe Right(('A', "C"))
    runParser("AC", parseAButThrowAwayB) shouldBe Left(
      "Error parsing 'B'. Unexpected 'C'")
    runParser("C", parseAButThrowAwayB) shouldBe Left(
      "Error parsing 'A'. Unexpected 'C'")
  }

  it should "match a parser but throw it away for !>>" in {
    val parseAAndBButThrowAwayA = parserA !>> parserB
    runParser("ABC", parseAAndBButThrowAwayA) shouldBe Right(('B', "C"))
    runParser("AC", parseAAndBButThrowAwayA) shouldBe Left(
      "Error parsing 'B'. Unexpected 'C'")
    runParser("C", parseAAndBButThrowAwayA) shouldBe Left(
      "Error parsing 'A'. Unexpected 'C'")
  }

  it should "match string between quotes" in {
    val pDoubleQuote = parseChar('"')
    val pSingleQuote = parseChar('\'')
    val parseLower = **(parseLowerCase) // one or more lowercase characters

    val betweenParser1 = inBetween(pDoubleQuote, parseLower, pDoubleQuote)
    val input1 = "\"helloworld\""
    runParser(input1, betweenParser1) shouldBe Right(("helloworld".toList, ""))

    val betweenParser2 = inBetween(pSingleQuote, parseLower, pSingleQuote)
    val input2 = "'helloworld'"
    runParser(input2, betweenParser2) shouldBe Right(("helloworld".toList, ""))
  }

  it should "match everything inside a paragraph tag" in {
    val pTagStart = parseString("<p>")
    val pTagEnd = parseString("</p>")
    val pWhiteSpaceAndLowerCase =
      **(anyCharOf(('a' to 'z').toList ++ List(' ')))

    val betweenParser = inBetween(pTagStart, pWhiteSpaceAndLowerCase, pTagEnd)

    val input = "<p>this is html</p>"

    runParser(input, betweenParser) shouldBe Right(("this is html".toList, ""))
  }

  it should "find one or more characters separated by a separator" in {
    val comma = parseChar(',')
    val digit = parseDigit
    val digitSeparatedByOneOrMoreComma = sepBy1(digit, comma)

    runParser("12,3,4,5", digitSeparatedByOneOrMoreComma) shouldBe Left(
      "Error parsing ','. Unexpected '2'")

    val parsedOutput = runParser("1,2,3,4,5", digitSeparatedByOneOrMoreComma)

    parsedOutput.isRight shouldBe true

    val Right((matched, remaining)) = parsedOutput

    remaining shouldBe empty

    matched shouldBe (List(('1', ','), ('2', ','), ('3', ','), ('4', ',')), '5')
  }

  it should "find zero or more characters separated by separator" in {
    val comma = parseChar(',')
    val digit = parseDigit
    val digitSeparatedByZeroOrMoreComma = sepBy(digit, comma)

    val parsedOutput1 = runParser("1,2,3,4,5", digitSeparatedByZeroOrMoreComma)
    val parsedOutput2 = runParser("12,3,4,5", digitSeparatedByZeroOrMoreComma)
    val parsedOutput3 = runParser("123,,5", digitSeparatedByZeroOrMoreComma)

    parsedOutput1 shouldBe 'right
    parsedOutput2 shouldBe 'right
    parsedOutput3 shouldBe 'right

    parsedOutput1.right.get._1 shouldBe
      (List(('1', ','), ('2', ','), ('3', ','), ('4', ',')), '5')

    parsedOutput2 shouldBe Right((List(), '1'), "2,3,4,5")
  }

}
