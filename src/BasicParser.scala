class BasicParser {
  def parseChar(char: String)(
      input: String): Either[String, (String, String)] = {
    if (input == null || input.isEmpty) Left("No more input")
    else if (input.startsWith(char)) Right(input.substring(char.length), char)
    else Left("Didn't match")
  }
}
