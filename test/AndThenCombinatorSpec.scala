import BasicParser._
import ParserCombinator._

class AndThenCombinatorSpec extends TestBase("AndThenCombinator") {

  val parserA: Parser[Char] = Parser(parseChar('A'))
  val parserB: Parser[Char] = Parser(parseChar('B'))
  val andThenCombined: Parser[(Char, Char)] = andThen(parserA, parserB)

  it should "return output of parser2 when parser1 is successful" in {
    runParser("ABCD", andThenCombined) shouldBe Right(('A', 'B'), "CD")
  }

  it should "return failure when parser1 fails" in {
    runParser("BCD", andThenCombined) shouldBe Left("Expecting 'A'. Got 'B'")
  }

  it should "return failure when parser2 fails" in {
    runParser("ACD", andThenCombined) shouldBe Left("Expecting 'B'. Got 'C'")
  }

  it should "return No more input when input is empty" in {
    runParser("", andThenCombined) shouldBe Left("No more input")
  }

  it should "return failure when the input to second parser is empty" in {
    runParser("A", andThenCombined) shouldBe Left("No more input")
  }

}
