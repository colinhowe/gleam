package gleam.compiler

object StringHandler {
  def parseString(value : String) : String = {
    val lines = value.replaceAll("\r\n", "\n").split("\r|\n")
    
    // This is a multi-line string if it has a new-line straight away
    val text = if (lines(0) == "\"") {
      // Determine indentation from the last line - and then 1 spaces more
      val indentation = lines(lines.length - 1).indexOf("\"") + 2
      
      // Remove the indentation from each line and build up a string to output
      val outputStringBuffer = new StringBuffer()
      
      for (line <- lines.slice(1, lines.length - 1)) {
        outputStringBuffer.append(line.substring(Math.min(line.length, indentation)) + "\n")
      }
      
      // Remove the trailing + and quote and new line
      outputStringBuffer.substring(0, outputStringBuffer.length() - 1)
    } else {
      value.substring(1, value.length - 1)
    }
    text.replaceAll("\\\\\"", "\"")
  }
}