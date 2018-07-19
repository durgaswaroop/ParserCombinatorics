import BasicParser._

object ParserCombinator {

  /**
    * 1. Run the first parser on the given input.
    *   - If there is a failure, just return from there.
    * 2. If the first parsing succeeds, run the second parser on the remaining
    * input
    *   - If there is a failure, just return from here too.
    * 3. If both the parsers succeed, return a tuple of both the parser values.
    */
  def andThen[T](parser1: Parser[T], parser2: Parser[T]): Parser[(T, T)] = {

    def innerFunc(input: String) = {
      val firstParse = runParser(input, parser1)

      firstParse match {
        case Left(value1) => Left(value1)
        case Right((matched1, remaining1)) =>
          val secondParse = runParser(remaining1, parser2)

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
  def orElse[T](parser1: Parser[T], parser2: Parser[T]): Parser[T] = {

    def innerFunc(input: String) = {
      val firstParse = runParser(input, parser1)
      firstParse match {
        case Right(_) => firstParse
        case Left(_)  => runParser(input, parser2)
      }
    }

    Parser(innerFunc)
  }
}
