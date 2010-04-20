package gleam.compiler

import org.junit.Test
import org.junit.Assert._

class TestStringHandler {
  
  @Test
  def basicMultiLine = {
    val result = StringHandler.parseString(
        "\"\"\n" +
        "  hi\n" +
        "  there\n" +
        "\"\"")
        
    assertEquals("hi\nthere", result)
  }
  
  @Test
  def unsufficientIndentation = {
    val result = StringHandler.parseString(
        "\"\"\n" +
        "  hi\n" +
        "\n" +
        "  there\n" +
        "\"\"")
        
    assertEquals("hi\n\nthere", result)
  }
  
  @Test
  def simpleString = {
    val result = StringHandler.parseString("\"hi there\"")
    assertEquals("hi there", result)
  }
}