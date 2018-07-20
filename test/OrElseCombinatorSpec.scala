import BasicParser._

class OrElseCombinatorSpec extends TestBase("OrElseCombinatorSpec") {
  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')
  val parserC: Parser[Char] = parseChar('C')

  val parserAOrB: Parser[Char] = parserA <|> parserB
  val parserAOrBOrC: Parser[Char] = parserA <|> parserB <|> parserC

  it should "return if the first parser passes" in {
    runParser("ABC", parserAOrB) shouldBe Right('A', "BC")
  }

  it should "return the output of second parser if first parsers fails" in {
    runParser("BC", parserAOrB) shouldBe Right('B', "C")
    runParser("BZZ", parserAOrB) shouldBe Right('B', "ZZ")
    runParser("CB", parserAOrB) shouldBe Left("Expecting 'B'. Got 'C'")
    runParser("CZZ", parserAOrB) shouldBe Left("Expecting 'B'. Got 'C'")
  }

  it should "return that no more chars are present for empty string" in {
    runParser("", parserAOrB) shouldBe Left("No more input")
  }

  it should "succeed for three parsers in series for input ABC" in {
    runParser("A", parserAOrBOrC) shouldBe Right('A', "")
    runParser("B", parserAOrBOrC) shouldBe Right('B', "")
    runParser("C", parserAOrBOrC) shouldBe Right('C', "")
  }

  it should "fail for three parsers in series for input XYZ" in {
    runParser("XYZ", parserAOrBOrC) shouldBe Left("Expecting 'C'. Got 'X'")
  }

}
