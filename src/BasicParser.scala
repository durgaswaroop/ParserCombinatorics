case class Parser[T](func: String => Either[String, (String, T)])

object BasicParser {
  def parseChar(char: Char)(input: String): Either[String, (String, Char)] = {
    if (input == null || input.isEmpty) Left("No more input")
    else if (input(0) == char) Right(input.substring(1), char)
    else Left("Didn't match")
  }

  def runParser[T](input: String, parser: Parser[T]) = parser.func(input)

}
