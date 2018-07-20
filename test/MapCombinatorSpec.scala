import BasicParser._

class MapCombinatorSpec extends TestBase("MapCombinator") {
  val parserA: Parser[Char] = Parser(parseChar('A'))
  val parserB: Parser[Char] = Parser(parseChar('B'))

  it should "transform parsed value with given function if parsing is successful" in {
    val f1: Char => Boolean = (x: Char) => x == 'A'
    val mParserAF1 = parserA |>> f1
    runParser("ABC", mParserAF1) shouldBe Right((true, "BC"))

    val f2: Char => Char = (x: Char) => x
    val mParserAF2 = parserA |>> f2
    runParser("ABC", mParserAF2) shouldBe Right(('A', "BC"))

    val f3: Char => String = (x: Char) => x.toString
    val mParserBF3 = parserB |>> f3
    runParser("BC", mParserBF3) shouldBe Right(("B", "C"))
  }

  it should "return if the parsing fails" in {
    val f1: Char => Boolean = (x: Char) => x == 'A'
    val mParserBF1 = parserB |>> f1
    runParser("ABC", mParserBF1) shouldBe Left("Expecting 'B'. Got 'A'")
    runParser("", mParserBF1) shouldBe Left("No more input")
  }
}
