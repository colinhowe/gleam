package gleam
import gleam.compiler.parser.ParserException

import java.io.BufferedReader
import java.io.PushbackReader
import java.io.StringReader
import java.io.FileOutputStream
import java.io.File

import gleam.compiler.parser.Parser
import gleam.compiler.lexer.Lexer
import gleam.compiler.lexer.LexerException
import gleam.compiler.GleamCompiler
import gleam.compiler.CompilationUnit
import gleam.compiler.Errored
import gleam.compiler.Parsed
import gleam.compiler.Finished
import gleam.compiler.IntermediateResult
import scala.actors.Actor

case class Parse(unit : CompilationUnit)

class ParserController(compiler : GleamCompiler, exceptionHandler : ExceptionHandler) extends CompilationController(exceptionHandler) {
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
        val parseMessage = e.getMessage().substring(closeBracket + 2).replaceAll("\n", "\\\\n");
        exceptionHandler ! CompilationException(ParseError(lineNumber, e.getToken.getPos, parseMessage))
        val result = new CompilationResult(message.unit.getSourceName, null)
        result.addError(ParseError(lineNumber, e.getToken.getPos, parseMessage))
        compiler ! result
      case e : LexerException =>
        // Expect the error to look like [line, position] blahblah
        val openBracket = e.getMessage().indexOf("[")
        val comma = e.getMessage().indexOf(",")
        val closeBracket = e.getMessage().indexOf("]")
        val lineNumber = Integer.parseInt(e.getMessage().substring(openBracket + 1, comma))
        val columnNumber = Integer.parseInt(e.getMessage().substring(comma + 1, closeBracket))
        val parseMessage = e.getMessage().substring(closeBracket + 2).replaceAll("\n", "\\\\n");
        exceptionHandler ! CompilationException(ParseError(lineNumber, columnNumber, parseMessage))
        val result = new CompilationResult(message.unit.getViewName, null)
        result.addError(ParseError(lineNumber, columnNumber, parseMessage))
        compiler ! result
    } finally {
      if (reader != null) {
        reader.close
      }
    }
  }
}