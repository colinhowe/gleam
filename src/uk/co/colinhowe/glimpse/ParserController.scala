package uk.co.colinhowe.glimpse

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
    case Finished() => exit
  }
  
  def parse(message : Parse) {
    // create lexer
    val lexer = new Lexer(new PushbackReader(new BufferedReader(new StringReader(message.unit.getSource)), 204800))
    
    // parse program
    val parser = new Parser(lexer)
    val ast = parser.parse()
  
    compiler ! Parsed(new IntermediateResult(ast, message.unit.getViewName, message.unit.getSourceName))
  }
}