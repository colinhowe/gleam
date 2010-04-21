package gleam.compiler

import org.junit.Test
import org.junit.Assert._

import gleam.CompilationException
import gleam.ParseError

class TestStringHandler {
  
  @Test
  def basicMultiLine = {
    val result = StringHandler.parseString(
        "\"\n" +
        "  hi\n" +
        "  there\n" +
        "\"")
        
    assertEquals("hi\nthere", result)
  }
  
  @Test
  def indentedMultiLine = {
    val result = StringHandler.parseString(
        "\"\n" +
        "    hi\n" +
        "    there\n" +
        "  \"")
        
    assertEquals("hi\nthere", result)
  }
  
  @Test
  def insufficientIndentation = {
    val result = StringHandler.parseString(
        "\"\n" +
        "  hi\n" +
        "\n" +
        "  there\n" +
        "\"")
        
    assertEquals("hi\n\nthere", result)
  }
  
  @Test
  def simpleString = {
    val result = StringHandler.parseString("\"hi there\"")
    assertEquals("hi there", result)
  }
  
  @Test
  def quoteEscaping = {
    val result = StringHandler.parseString("\"I said \\\"Work please\\\". And it worked.\"")
    assertEquals("I said \"Work please\". And it worked.", result)
  }
  
  @Test
  def backslashEscaping = {
    val result = StringHandler.parseString("\"Backslash \\\\ works\"")
    assertEquals("Backslash \\ works", result)
  }
  
  @Test
  def invalidEscaping = {
    try {
      StringHandler.parseString("\"Not escaped properly \\a\"")
      fail("Should have caused a compilation error")
    } catch {
      case e : IllegalArgumentException =>
        assertEquals("Unsupported escape sequence [\\a]", e.getMessage)
    }
  }
}