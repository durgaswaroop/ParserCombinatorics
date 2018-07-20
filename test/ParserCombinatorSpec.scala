import BasicParser._
import ParserCombinator._

class ParserCombinatorSpec extends TestBase("ParserCombinator") {
  it should "parse correctly when I combine orElse with andThen" in {
    val parserA = Parser(parseChar('A'))
    val parserB = Parser(parseChar('B'))
    val parserC = Parser(parseChar('C'))

    val bOrElseC = orElse(parserB, parserC)
    val aAndThenBorC = andThen(parserA, bOrElseC)

    runParser("ABZ", aAndThenBorC) shouldBe Right(('A', 'B'), "Z")
    runParser("ACZ", aAndThenBorC) shouldBe Right(('A', 'C'), "Z")
  }

  it should "parse correctly when I combine orElse with map" in {
    val parser1 = Parser(parseChar('1'))
    val parser2 = Parser(parseChar('2'))

    val aOrElseB = orElse(parser1, parser2)
    val asInt = (c: Char) => c.asDigit // toInt would give the ascii number
    val aOrElseBAsInt = map(aOrElseB, asInt)

    runParser("123", aOrElseBAsInt) shouldBe Right(1, "23")
  }

  it should "parse correctly when I combine andThen with map" in {
    val parserA = Parser(parseChar('A'))
    val parserB = Parser(parseChar('B'))

    //noinspection ScalaUnnecessaryParentheses
    val asString: ((Char, Char)) => String =
      (tup: (Char, Char)) => tup._1.toString + tup._2.toString

    val aThenB: Parser[(Char, Char)] = andThen(parserA, parserB)
    val aThenBAsString = map(aThenB, asString)

    runParser("ABC", aThenBAsString) shouldBe Right("AB", "C")

    val parser1 = Parser(parseChar('1'))
    val parser2 = Parser(parseChar('2'))

    val oneThenTwo = andThen(parser1, parser2)
    val oneThenTwoAsString = map(oneThenTwo, asString)

    runParser("12C", oneThenTwoAsString) shouldBe Right("12", "C")

    val asInt: String => Int = s => s.toInt
    val oneThenTwoAsStringAsInt = map(oneThenTwoAsString, asInt)

    runParser("12C", oneThenTwoAsStringAsInt) shouldBe Right(12, "C")
  }
}
