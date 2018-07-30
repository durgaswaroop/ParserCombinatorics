package parser

import parser.ParserCombinator.{choice, sepBy, sequence}

object CharCombinator {
  // Parse a character char in the input.
  def parseChar(char: Char): Parser[Char] = {
    def innerFunc(input: String) = {

      if (input == null || input.isEmpty) Left("No more input")
      else if (input(0) == char) Right(char, input.substring(1))
      else Left(s"Error parsing '$char'. Unexpected '${input(0)}'")
    }

    Parser(innerFunc)
  }

  def anyCharOf(chars: List[Char]): Parser[Char] = {
    val parsers: List[Parser[Char]] = chars map parseChar
    choice(parsers)
  }

  def parseLowerCase: Parser[Char] = anyCharOf(('a' to 'z').toList)

  def parseDigit: Parser[Char] = anyCharOf(('0' to '9').toList)

  def allCharsOf(chars: List[Char]): Parser[List[Char]] = {
    val parsers: List[Parser[Char]] = chars map parseChar
    sequence(parsers)
  }

  // Parses a char string like "1,2,3,4" into a List(1,2,3,4)
  def parseOneOrMoreDigitAsList[U](parser: Parser[Char],
                                   separator: Parser[U]): Parser[List[Int]] = {
    sepBy(parser, separator) |>> (t => t._1.map(_._1.asDigit) :+ t._2.asDigit)
  }

}
