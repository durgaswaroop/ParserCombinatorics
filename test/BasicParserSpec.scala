import BasicParser._

class BasicParserSpec extends TestBase("BasicParser") {

  it should "return a Either object for the input" in {
    val pchara = parseChar('a') _
    pchara("abc") shouldBe an[Either[String, (String, Char)]]
  }

  it should
    "return remaining input & the matched character if the char is matched" in {
    val pcharA = parseChar('A') _
    pcharA("A").merge shouldBe ("", 'A')

  }

  it should "return a failure msg in case of no match" in {
    val pcharC = parseChar('C') _
    pcharC("AB").merge shouldBe "Expecting 'C'. Got 'A'"
  }

  it should "return No more input for empty and null string inputs" in {
    val pchar = parseChar('H') _
    parseChar('H')("").merge shouldBe "No more input"
    parseChar('H')(null).merge shouldBe "No more input"
  }

  it should "run the parser on the given input" in {
    val parseH = Parser(parseChar('H'))
    runParser("Hello", parseH).merge shouldBe ("ello", 'H')
    runParser("hola", parseH).merge shouldBe "Expecting 'H'. Got 'h'"
    runParser("", parseH).merge shouldBe "No more input"
  }

}
