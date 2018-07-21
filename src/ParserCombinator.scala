object ParserCombinator {
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

  def anyOf(chars: List[Char]): Parser[Char] = {
    val parsers: List[Parser[Char]] = chars map parseChar
    choice(parsers)
  }

  def parseLowerCase: Parser[Char] = anyOf(('a' to 'z').toList)

  def parseDigit: Parser[Char] = anyOf(('0' to '9').toList)

  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] = {

    def concatMatches(parser1: Parser[List[T]],
                      parser2: Parser[List[T]]): Parser[List[T]] = {
      val func = (tuple: (List[T], List[T])) => tuple._1 ++ tuple._2
      parser1 >> parser2 |>> func
    }

    def listMapper = (value: T) => List(value)

    parsers.map(_ |>> listMapper).reduce(concatMatches)
  }

  def allOf(chars: List[Char]): Parser[List[Char]] = {
    val parsers: List[Parser[Char]] = chars map parseChar
    sequence(parsers)
  }

  def parseString(stringToMatch: String): Parser[String] =
    allOf(stringToMatch.toList) |>> (_.mkString)

  def parseZeroOrMore[T](parser: Parser[T],
                         input: String): (List[T], String) = {
    val firstRun = runParser(input, parser)
    firstRun match {
      case Left(_) => (List.empty, input)
      case Right((matched, remaining)) =>
        val (nextValue, nextRemaining) = parseZeroOrMore(parser, remaining)
        val values = matched :: nextValue
        (values, nextRemaining)
    }
  }

  // Zero or more
  def **[T](parser: Parser[T]): Parser[String] = {
    def innerFunc(input: String) = {
      Right(parseZeroOrMore(parser, input))
    }

    Parser(innerFunc) |>> (_.mkString)
  }

  def ++[T](parser: Parser[T]): Parser[String] = {
    def innerFunc(input: String) = {
      val firstRun = runParser(input, parser)
      firstRun match {
        case Left(value) => Left(value)
        case Right((matched, remaining)) =>
          val (nextMatched, nextRemaining) = parseZeroOrMore(parser, remaining)
          Right(matched :: nextMatched, nextRemaining)
      }
    }
    Parser(innerFunc) |>> (_.mkString)
  }

  def ??[T](parser: Parser[T]): Parser[String] = {
    def innerFunc(input: String) = {
      runParser(input, parser) match {
        case Left(_)      => Right("", input)
        case Right(value) => Right(value)
      }
    }
    Parser(innerFunc) |>> (_.toString)
  }

}
