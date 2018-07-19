object ParserCombinator {
  def andThen[T](parser1: Parser[T], parser2: Parser[T])(input: String) = {

    val firstParse = parser1.func(input)

    firstParse match {
      case Left(_) => firstParse
      case Right((matched1, remaining1)) =>
        val secondParse = parser2.func(remaining1)

        secondParse match {
          case Left(_) => secondParse
          case Right((matched2, remaining2)) =>
            Right((matched1, matched2), remaining2)
        }

    }

  }

}
