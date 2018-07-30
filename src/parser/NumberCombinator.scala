package parser

import parser.CharCombinator.{parseChar, parseDigit}
import parser.ParserCombinator.{++, ??, runParser}

object NumberCombinator {

  def parseInt: Parser[Int] = {
    def innerFunc(input: String) = {
      val sign = ??(parseChar('-')) |>> (_.mkString)
      val digits = ++(parseDigit) |>> (_.mkString)
      val optSignThenDigits =
        sign >> digits |>> (t => t._1.mkString + t._2.mkString)

      val parsed = runParser(input, optSignThenDigits)
      parsed match {
        case Left(value) => Left(value)
        case Right((matched, remaining)) =>
          if (remaining.isEmpty) Right(matched, "")
          else {
            val secondParse = runParser(remaining, digits)
            secondParse match {
              case Left(value) => Left(value)
            }
          }
      }
    }

    parser.Parser(innerFunc) |>> (_.toInt)
  }

  def parseFloat: Parser[Float] = {
    val optionalSign: Parser[String] = ??(parseChar('-')) |>> (_.mkString)
    val numbers: Parser[String] = ++(parseDigit) |>> (_.mkString)
    val optionalDot: Parser[String] = ??(parseChar('.')) |>> (_.mkString)
    val parser: Parser[(((String, String), String), String)] =
      optionalSign >> numbers >> optionalDot >> numbers
    parser |>> {
      case (((a, b), c), d) => (a + b + c + d).toFloat
    }
  }

}
