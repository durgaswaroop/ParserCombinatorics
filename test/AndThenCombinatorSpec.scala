import BasicParser._
import ParserCombinator._

class AndThenCombinatorSpec extends TestBase("AndThenCombinator") {

  val parserA: Parser[Char] = Parser(parseChar('A'))
  val parserB: Parser[Char] = Parser(parseChar('B'))
  val andThenCombined = andThen(parserA, parserB) _

  it should "return output of parser2 when parser1 is successful" in {
    andThenCombined("ABCD") shouldBe Right(('A', 'B'), "CD")
  }

  it should "return failure when parser1 fails" in {
    andThenCombined("BCD") shouldBe Left("Expecting 'A'. Got 'B'")
  }

  it should "return failure when parser2 fails" in {
    andThenCombined("ACD") shouldBe Left("Expecting 'B'. Got 'C'")
  }

  it should "return No more input when input is empty" in {}

}
