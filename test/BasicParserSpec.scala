import BasicParser._

class BasicParserSpec extends TestBase("BasicParser") {

  it should "return a Either object for the input" in {
    parseChar('a')("abc") shouldBe an[Either[_, _]]
  }

  it should
    "return remaining input & the matched character if the char is matched" in {
    parseChar('A')("A").merge shouldBe ("", 'A')

  }

  it should "return a failure msg in case of no match" in {
    parseChar('C')("AB").merge shouldBe "Didn't match"
  }

  it should "return No more input for empty and null string inputs" in {
    parseChar('H')("").merge shouldBe "No more input"
    parseChar('H')(null).merge shouldBe "No more input"
  }

  it should "run the parser on the given input" in {
    val parseH = Parser(parseChar('H'))
    runParser("Hello", parseH).merge shouldBe ("ello", 'H')
    runParser("hola", parseH).merge shouldBe "Didn't match"
  }

}
