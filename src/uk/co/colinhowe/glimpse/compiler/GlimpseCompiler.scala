package uk.co.colinhowe.glimpse.compiler

import java.io.BufferedReader
import java.io.File
import java.io.PushbackReader
import java.io.StringReader
import java.lang.Runtime
import java.util.concurrent.Callable
import java.util.concurrent.Executors

import scala.collection.JavaConversions._
import scala.collection.JavaConversions
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel
import scala.actors.Futures._
import scala.collection.mutable.Buffer

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.CompilationResult
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer
import uk.co.colinhowe.glimpse.compiler.node.Start
import uk.co.colinhowe.glimpse.compiler.parser.Parser

case class Join
case class Joined

class GlimpseCompiler extends Actor {
  
  var classPathResolver : ClassPathResolver = null
  
  case class Parsed(result : IntermediateResult)
  case class Finished
  
  val startTime = System.currentTimeMillis
  val toParse = Buffer[IntermediateResult]()
  val toProcess = Buffer[IntermediateResult]()
  var classPathResolved = false
  var resultsReady = false
  var sourcesRemaining = 0
  
  val results = Buffer[CompilationResult]()
  val resultsActor = new Actor {
    var finished = false
    def act() {
      var replyTo : OutputChannel[Any] = null
      loop {
        react {
          case result : CompilationResult => 
            results += result
          case Join() =>
            if (!finished) {
              replyTo = sender
            } else {
              replyTo ! Joined()
              exit
            }
          case Finished() =>
            finished = true
            if (replyTo != null) {
              replyTo ! Joined()
              exit
            }
          case _ =>
        }
      }
    }
  }
  resultsActor.start
  
  private def debug(line : String) {
    val time = System.currentTimeMillis - startTime
    println("[" + time + "] " + line)
  }
  
  def act() {
    loop {
      react {
        case ClassPathResolver.Resolved => 
          classPathResolved = true
          toParse.foreach(GlimpseCompiler.this ! Parsed(_))
          
        case Parsed(result) => 
          if (!classPathResolved) {
            toParse += result
          } else {
            toProcess += result
            sourcesRemaining -= 1
          }
          
          if (sourcesRemaining == 0) {
            // Start real processing now
            compileAsts(toProcess)
            exit
          }
      }
    }
  }
  
  private def compileAsts(intermediates : Iterable[IntermediateResult]) {
    
    /*
     * Stage 1
     * Matching line numbers to nodes
     * Typing of nodes
     * Finding macro definitions
     */
    val typeProvider = new TypeProvider()
    val macroProvider = new MacroDefinitionProvider()
    val typeResolver = new TypeResolver(typeProvider, macroProvider)
    val callResolver = new CallResolver(macroProvider)
    val futures = Buffer[scala.actors.Future[_]]()
    
    for (intermediate <- intermediates) {
      futures += future {
        val ast = intermediate.ast
        intermediate.typeNameResolver = new TypeNameResolver(ast, classPathResolver)
        ast.apply(intermediate.lineNumberProvider)

        val finder = new MacroDefinitionFinder(intermediate.lineNumberProvider, typeProvider, macroProvider, intermediate.typeNameResolver)
        ast.apply(finder)
        intermediate.errors ++ finder.errors

        intermediate.ast.apply(typeResolver)
      }
    }
    for (future <- futures) {
      future()
    }
    macroProvider !? Join()
    typeResolver.stop
    
    /*
     * Stage 2
     * Check type safety
     * Produce byte code
     * 
     * TODO Bundle all java files together and output in one go
     */
    futures.clear()
    for (intermediate <- intermediates) {
      futures += future {
        val viewname = intermediate.viewName.replaceAll("-", "_")
        val ast = intermediate.ast
        
        val errors = scala.collection.mutable.Buffer[CompilationError]() ++ intermediate.errors
        
        // Run the type checker
        val typeChecker = new TypeChecker(intermediate.lineNumberProvider, macroProvider, typeResolver, intermediate.typeNameResolver, callResolver)
        ast.apply(typeChecker)
        errors ++ typeChecker.errors
        
        // Compile all the nodes down to java
        val bcp = new ByteCodeProducer(
            viewname, 
            intermediate.lineNumberProvider, 
            typeResolver, 
            "temp/" + viewname + ".class", 
            intermediate.typeNameResolver, 
            intermediate.sourcename,
            callResolver,
            typeChecker.resolvedCallsProvider)
        ast.apply(bcp)
        errors ++ bcp.errors
        
        // Output the errors
        for (error <- errors) {
          System.out.println(error)
        }
        
        // Output the view as a file only if needed
        val file = new File("temp/" + viewname + ".class")
        val result = new CompilationResult(viewname, file)
        for (error <- errors) {
          result.addError(error)
        }
        
        resultsActor ! result
        
//        debug("Compiled " + viewname)
      }
    }
    
    for (future <- futures) {
      future()
    }
    
    resultsActor ! Finished()
  }
  

  def compile(units : java.util.List[CompilationUnit]) : java.util.List[CompilationResult] = {
    compile(units, new java.util.LinkedList[String]())
  }
  
  def compile(units : java.util.List[CompilationUnit], classPaths : java.util.List[String]) : java.util.List[CompilationResult] = {
    sourcesRemaining = units.size
    this.start()

    classPathResolver = new ClassPathResolver(classPaths, this)

    // Parse each source file
    for (unit <- units) {
      val parser = future {
        // create lexer
        val lexer = new Lexer(new PushbackReader(new BufferedReader(new StringReader(unit.getSource())), 204800))
        
        // parse program
        val parser = new Parser(lexer)
        val ast = parser.parse()
        
        GlimpseCompiler.this ! Parsed(new IntermediateResult(ast, unit.getViewName(), unit.getSourceName))
      }
    }
    
    resultsActor !? Join()
//    Waiter.waitForActors(List(resultsActor))


    results
  }
}
