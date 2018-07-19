import BasicParser._
import org.scalatest.EitherValues

class BasicParserSpec extends TestBase("BasicParser") {

  it should "return a Either object for the input" in {
    EitherValues
    parseChar("a")("abc") shouldBe an[Either[_, _]]
  }

  it should
    "return remaining input & the matched character if the char is matched" in {
    parseChar("A")("A") === ("", "A")
    parseChar("he")("hello") === ("llo", "he")

  }

  it should "return a failure msg in case of no match" in {
    parseChar("C")("AB") === "Didn't match"
  }

  it should "return No more input for empty and null string inputs" in {
    parseChar("H")("") === "No more input"
    parseChar("H")(null) === "No more input"
  }

}
