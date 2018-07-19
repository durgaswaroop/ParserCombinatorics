import BasicParser._
import ParserCombinator._

class ParserCombinatorSpec extends TestBase("ParserCombinator") {

  val parserA: Parser[Char] = Parser(parseChar('A'))
  val parserB: Parser[Char] = Parser(parseChar('B'))

  it should "return output of parser2 when parser1 is successful" in {
    andThen(parserA, parserB)("ABCD") shouldBe Right(('A', 'B'), "CD")
  }

  it should "return failure when parser1 fails" in {
    andThen(parserA, parserB)("BCD") shouldBe Left("Expecting 'A'. Got 'B'")
  }

}
