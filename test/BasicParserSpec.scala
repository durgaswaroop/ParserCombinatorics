import org.scalatest.EitherValues

class BasicParserSpec extends TestBase("BasicParser") {

  val basicParser = new BasicParser()

  it should "return a Either object for the input" in {
    EitherValues
    basicParser.parseChar("a")("abc") shouldBe an[Either[_, _]]
  }

  it should
    "return remaining input & the matched character if the char is matched" in {
    basicParser.parseChar("A")("A") === ("", "A")
    basicParser.parseChar("he")("hello") === ("llo", "he")

  }

  it should "return a failure msg in case of no match" in {
    basicParser.parseChar("C")("AB") === "Didn't match"
  }

  it should "return No more input for empty and null string inputs" in {
    basicParser.parseChar("H")("") === "No more input"
    basicParser.parseChar("H")(null) === "No more input"
  }

}
