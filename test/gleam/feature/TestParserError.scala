package gleam.feature

import org.junit.Test
import org.junit.Assert._

import gleam.ParseError
import gleam.compiler.CompilerTest

class TestParserError extends CompilerTest {
  
  @Test
  def unknownToken = {
    """
    p {
      "
    """ failsWith
    ParseError(
      line = 3,
      column = 7,
      message = "Unknown token: \"\\n    "
    )
  }
  
  @Test
  def lexerError = {
    """
    node node node node
    """ failsWith
    ParseError(
      line = 2,
      column = 10,
      message = "expecting: newline, identifier"
    )
  }
}
