import BasicParser.{parseChar, runParser}
import ParserCombinator.orElse

class OrElseCombinatorSpec extends TestBase("OrElseCombinatorSpec") {
  val parserA: Parser[Char] = Parser(parseChar('A'))
  val parserB: Parser[Char] = Parser(parseChar('B'))
  val orElseCombined: Parser[Char] = orElse(parserA, parserB)

  it should "return if the first parser passes" in {
    runParser("ABC", orElseCombined) shouldBe Right('A', "BC")
  }

  it should "return the output of second parser if first parsers fails" in {
    runParser("BC", orElseCombined) shouldBe Right('B', "C")
    runParser("CB", orElseCombined) shouldBe Left("Expecting 'B'. Got 'C'")
  }

  it should "return that no more chars are present for empty string" in {
    runParser("", orElseCombined) shouldBe Left("No more input")
  }
}
