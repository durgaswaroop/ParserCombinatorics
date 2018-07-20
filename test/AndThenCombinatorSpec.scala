import ParserCombinator._

class AndThenCombinatorSpec extends TestBase("AndThenCombinator") {

  val parserA: Parser[Char] = parseChar('A')
  val parserB: Parser[Char] = parseChar('B')
  val parserC: Parser[Char] = parseChar('C')

  val parserAB = parserA >> parserB
  val parserABC = parserA >> parserB >> parserC

  it should "return output of parser2 when parser1 is successful" in {
    runParser("ABCD", parserAB) shouldBe Right(('A', 'B'), "CD")
  }

  it should "return failure when parser1 fails" in {
    runParser("BCD", parserAB) shouldBe Left("Expecting 'A'. Got 'B'")
  }

  it should "return failure when parser2 fails" in {
    runParser("ACD", parserAB) shouldBe Left("Expecting 'B'. Got 'C'")
  }

  it should "return No more input when input is empty" in {
    runParser("", parserAB) shouldBe Left("No more input")
  }

  it should "return failure when the input to second parser is empty" in {
    runParser("A", parserAB) shouldBe Left("No more input")
  }

  it should "succeed for three parsers combined in series for input ABC" in {
    runParser("ABC", parserABC) shouldBe Right((('A', 'B'), 'C'), "")
  }

  it should "fail for three parsers combined in series" in {
    runParser("AB", parserABC) shouldBe Left("No more input")
    runParser("XYZ", parserABC) shouldBe Left("Expecting 'A'. Got 'X'")
  }

}
