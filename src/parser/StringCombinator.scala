package parser

import parser.CharCombinator.{allCharsOf, anyCharOf}
import parser.ParserCombinator.**

object StringCombinator {
  def parseString(stringToMatch: String): Parser[String] =
    allCharsOf(stringToMatch.toList) |>> (_.mkString)

  def parseWhiteSpace: Parser[String] = {
    val whiteSpaceChars = anyCharOf(List(' ', '\t', '\n', '\r'))
    **(whiteSpaceChars) |>> (_.mkString)
  }
}
