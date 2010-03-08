package uk.co.colinhowe.glimpse
import uk.co.colinhowe.glimpse.compiler.parser.ParserException

import java.io.BufferedReader
import java.io.PushbackReader
import java.io.StringReader
import java.io.FileOutputStream
import java.io.File

import uk.co.colinhowe.glimpse.compiler.parser.Parser
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer
import uk.co.colinhowe.glimpse.compiler.GlimpseCompiler
import uk.co.colinhowe.glimpse.compiler.CompilationUnit
import uk.co.colinhowe.glimpse.compiler.Errored
import uk.co.colinhowe.glimpse.compiler.Parsed
import uk.co.colinhowe.glimpse.compiler.Finished
import uk.co.colinhowe.glimpse.compiler.IntermediateResult
import scala.actors.Actor

case class Parse(unit : CompilationUnit)

class ParserController(compiler : GlimpseCompiler, exceptionHandler : ExceptionHandler) extends CompilationController(exceptionHandler) {
  def handleMessage = {
    case message : Parse => parse(message)
    case Finished() => 
      compiler ! Finished()
      exit
  }
  
  def parse(message : Parse) {
    // create lexer
    val reader = new PushbackReader(message.unit.getReader, 204800)
    val lexer = new Lexer(reader)
    
    // parse program
    val parser = new Parser(lexer)
    
    try {
      val ast = parser.parse()
  
      compiler ! Parsed(new IntermediateResult(ast, message.unit.getViewName, message.unit.getSourceName))
    } catch {
      case e : ParserException =>
        // Expect the error to look like [line, position] blahblah
        val openBracket = e.getMessage().indexOf("[")
        val comma = e.getMessage().indexOf(",")
        val closeBracket = e.getMessage().indexOf("]")
        val lineNumber = Integer.parseInt(e.getMessage().substring(openBracket + 1, comma))
        val parseMessage = e.getMessage().substring(closeBracket + 2)
        exceptionHandler ! CompilationException(ParseError(message.unit, lineNumber, parseMessage))
    } finally {
      if (reader != null) {
        reader.close
      }
    }
  }
}