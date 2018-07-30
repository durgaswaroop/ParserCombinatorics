package parser

object ParserCombinator {

  // Runs the given parser on the passed input string
  def runParser[T](input: String, parser: Parser[T]) = parser.func(input)

  // Returns parser1 <|> or parser2 <|> parser3 ...
  def choice[T](parsers: List[Parser[T]]): Parser[T] = parsers.reduce(_ <|> _)

  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] = {

    def concatMatches(parser1: Parser[List[T]],
                      parser2: Parser[List[T]]): Parser[List[T]] = {
      val func = (tuple: (List[T], List[T])) => tuple._1 ++ tuple._2
      parser1 >> parser2 |>> func
    }

    def listMapper = (value: T) => List(value)

    parsers.map(_ |>> listMapper).reduce(concatMatches)
  }

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
  def **[T](parser: Parser[T]): Parser[List[T]] = {
    def innerFunc(input: String) = {
      Right(parseZeroOrMore(parser, input))
    }

    Parser(innerFunc) //|>> (_.mkString)
  }

  def ++[T](parser: Parser[T]): Parser[List[T]] = {
    def innerFunc(input: String) = {
      val firstRun = runParser(input, parser)
      firstRun match {
        case Left(value) => Left(value)
        case Right((matched, remaining)) =>
          val (nextMatched, nextRemaining) = parseZeroOrMore(parser, remaining)
          Right(matched :: nextMatched, nextRemaining)
      }
    }

    Parser(innerFunc) //|>> (_.mkString)
  }

  def ??[T](parser: Parser[T]): Parser[List[T]] = {
    def innerFunc(input: String) = {
      runParser(input, parser) match {
        case Left(_)       => Right(List.empty[T], input)
        case Right((m, r)) => Right((List(m), r))
      }
    }

    Parser(innerFunc) //|>> (_.toString)
  }

  def between[U, T, V](parser1: Parser[U],
                       parser2: Parser[T],
                       parser3: Parser[V]): Parser[T] =
    parser1 !>> parser2 >>! parser3

  //One or more Parsers separated by the separator
  def sepBy1[T, U](parser: Parser[T],
                   separator: Parser[U]): Parser[(List[(T, U)], T)] =
    ++(parser >> separator) >> parser

  //One or more Parsers separated by the separator
  def sepBy[T, U](parser: Parser[T],
                  separator: Parser[U]): Parser[(List[(T, U)], T)] = {
    **(parser >> separator) >> parser
  }
}
