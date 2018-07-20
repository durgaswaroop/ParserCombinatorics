object BasicParser {
  // Parse a character char in the input.
  def parseChar(char: Char)(input: String): Either[String, (Char, String)] = {
    if (input == null || input.isEmpty) Left("No more input")
    else if (input(0) == char) Right(char, input.substring(1))
    else Left(s"Expecting '$char'. Got '${input(0)}'")
  }

  // Runs the given parser on the passed input string
  def runParser[T](input: String, parser: Parser[T]) = parser.func(input)

}
