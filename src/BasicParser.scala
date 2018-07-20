object BasicParser {
  // Parse a character char in the input.
  def parseChar(char: Char): Parser[Char] = {
    def innerFunc(input: String) = {
      if (input == null || input.isEmpty) Left("No more input")
      else if (input(0) == char) Right(char, input.substring(1))
      else Left(s"Expecting '$char'. Got '${input(0)}'")
    }
    Parser(innerFunc)
  }

  // Runs the given parser on the passed input string
  def runParser[T](input: String, parser: Parser[T]) = parser.func(input)

  // Returns parser1 <|> or parser2 <|> parser3 ...
  def choice[T](parsers: List[Parser[T]]): Parser[T] = parsers.reduce(_ <|> _)

}
