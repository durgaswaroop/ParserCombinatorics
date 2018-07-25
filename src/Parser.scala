import ParserCombinator.runParser

case class Parser[T](func: String => Either[String, (T, String)]) {

  /**
    * 1. Run the first parser on the given input.
    *   - If there is a failure, just return from there.
    * 2. If the first parsing succeeds, run the second parser on the remaining
    * input
    *   - If there is a failure, just return from here too.
    * 3. If both the parsers succeed, return a tuple of both the parser values.
    */
  def >>[U](other: Parser[U]): Parser[(T, U)] = {
    def innerFunc(input: String) = {
      val firstParse = runParser(input, this)

      firstParse match {
        case Left(value1) => Left(value1)
        case Right((matched1, remaining1)) =>
          val secondParse = runParser(remaining1, other)

          secondParse match {
            case Left(value2) => Left(value2)
            case Right((matched2, remaining2)) =>
              Right((matched1, matched2), remaining2)
          }
      }
    }

    Parser(innerFunc)
  }

  /**
    * 1. Run the first parser on the input data
    * 2. On Success, return the parser value and the remaining input
    * 3. If first parser fails, run the second parser on the original input
    * and return incase of success or failure
    *
    */
  def <|>(other: Parser[T]): Parser[T] = {
    def innerFunc(input: String) = {
      val firstParse = runParser(input, this)
      firstParse match {
        case Right(_) => firstParse
        case Left(_)  => runParser(input, other)
      }
    }

    Parser(innerFunc)
  }

  /**
    * Run a function on the Parser. If the parser is successful, run the
    * function on the matched output or else fail.
    */
  def |>>[U](func: T => U): Parser[U] = {
    def innerFunc(input: String): Either[String, (U, String)] = {
      val parserOutput: Either[String, (T, String)] = runParser(input, this)
      parserOutput match {
        case Right((matched, remaining)) => Right((func(matched), remaining))
        case Left(value)                 => Left(value)
      }
    }

    Parser(innerFunc)
  }

  // Parse current parser and another parser but ignore
  // what's matched by the other parser
  def >>![U](other: Parser[U]): Parser[T] = {
    def innerFunc(input: String) = {
      val firstParse = runParser(input, this)
      firstParse match {
        case Left(value) => Left(value)
        case Right((matched, remaining)) =>
          val secondParse = runParser(remaining, other)
          secondParse match {
            case Left(value)                 => Left(value)
            case Right((_, secondRemaining)) => Right(matched, secondRemaining)
          }
      }
    }

    Parser(innerFunc)
  }

  // Parse current parser and another parser but ignore
  // what's matched by the current parser
  def !>>[U](other: Parser[U]): Parser[U] = {
    def innerFunc(input: String) = {
      val firstParse = runParser(input, this)
      firstParse match {
        case Left(value) => Left(value)
        case Right((_, remaining)) =>
          val secondParse = runParser(remaining, other)
          secondParse match {
            case Left(value) => Left(value)
            case Right((matched, secondRemaining)) =>
              Right(matched, secondRemaining)
          }
      }
    }

    Parser(innerFunc)
  }

}
