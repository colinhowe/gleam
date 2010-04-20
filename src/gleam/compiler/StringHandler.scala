package gleam.compiler

object StringHandler {
  def parseString(value : String) : String = {
    
    val sep = "\"\""
    
    if (value.startsWith(sep)) {
      val lines = value.split("\n")
      
      // Determine indentation from the last line - and then 2 spaces more
      val indentation = lines(lines.length - 1).indexOf(sep) + 2
      
      // Remove the indentation from each line and build up a string to output
      val outputStringBuffer = new StringBuffer()
      
      for (line <- lines.slice(1, lines.length - 1)) {
        outputStringBuffer.append(line.substring(Math.min(line.length, indentation)) + "\n")
      }
      
      // Remove the trailing + and quote and new line
      val text = outputStringBuffer.substring(0, outputStringBuffer.length() - 1)
      return text
    } else {
      return value.replaceAll("\"", "")
    }
  }
}