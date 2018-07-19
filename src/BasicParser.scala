case class Parser[T](func: String => Either[String, (String, T)])

object BasicParser {
  // Parse a character char in the input.
  def parseChar(char: Char)(input: String): Either[String, (String, Char)] = {
    if (input == null || input.isEmpty) Left("No more input")
    else if (input(0) == char) Right(input.substring(1), char)
    else Left(s"Expecting '$char'. Got '${input(0)}'")
  }

  // Runs the given parser on the passed input string
  def runParser[T](input: String, parser: Parser[T]) = parser.func(input)

}
