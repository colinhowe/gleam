package gleam.compiler

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

import gleam.CompilationController
import gleam.CompilationException
import gleam.CompilationError
import gleam.CompilationResult
import gleam.compiler.lexer.Lexer
import gleam.compiler.node.Start
import gleam.compiler.parser.Parser
import scala.actors.Future
import gleam.ClassOutputter
import gleam.ParserController
import gleam.ExceptionHandler
import gleam.Parse

case class Join()
case class Joined()

case class Parsed(result : IntermediateResult)
case class Errored(e : Throwable)
case class Finished()

class GleamCompiler extends Actor {
  
  var classPathResolver : ClassPathResolver = null
  
  val startTime = System.currentTimeMillis
  val toParse = Buffer[IntermediateResult]()
  val toProcess = Buffer[IntermediateResult]()
  var classPathResolved = false
  var resultsReady = false
  private val errorHandler = new ExceptionHandler
  errorHandler.start
  val classOutputter = new ClassOutputter("temp/", errorHandler)
  classOutputter.start

  val results = Buffer[CompilationResult]()
  val resultsActor = new Actor {
    var exception : Throwable = null
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
              debug("Sending Joined whilst handling Join " + replyTo)
              sender ! Joined()
              exit
            }
          case Finished() =>
            finished = true
            if (replyTo != null) {
              debug("Sending Joined whilst handling Finished " + replyTo)
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
    println("[" + Thread.currentThread + "] [" + time + "] " + line)
  }
  
  var finished = false
  
  def act() {
    loop {
      react {
        
        case result : CompilationResult =>
          resultsActor ! result
        
        case ClassPathResolver.Resolved => 
          classPathResolved = true
          if (finished) {
            compileAsts(toProcess)
            exit
          }          
          
        case Errored(e) =>
          e.printStackTrace
          
        case Finished() =>
          finished = true
          if (classPathResolved) {
            compileAsts(toProcess)
            exit
          }          
          
        case Parsed(result) => 
          toProcess += result
      }
    }
  }
  def future(body : => Any) : scala.actors.Future[Any] = {
    scala.actors.Futures.future {
      try {
        body
      } catch {
        case e => e
      }
    }
  }
  
  private class Phase2Controller(errorHandler : ExceptionHandler) extends CompilationController(errorHandler) {
    def handleMessage = {
      case intermediate : IntermediateResult =>
        println("Intermediate: " + intermediate)
        try {
          val viewname = intermediate.viewName.replaceAll("-", "_")
          val ast = intermediate.ast
          
          val errors = scala.collection.mutable.Buffer[CompilationError]() ++ intermediate.errors
          
          // Run the type checker
          val typeChecker = new TypeChecker(intermediate.lineNumberProvider, macroProvider, typeResolver, intermediate.typeNameResolver, callResolver)
          ast.apply(typeChecker)
          errors ++= typeChecker.errors
          
          // Compile all the nodes down to java
          val bcp = new ByteCodeProducer(
              viewname, 
              intermediate.lineNumberProvider, 
              typeResolver, 
              intermediate.typeNameResolver, 
              intermediate.sourcename,
              callResolver,
              typeChecker.resolvedCallsProvider,
              classOutputter,
              macroProvider)
          ast.apply(bcp)
          errors ++= bcp.errors
          
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
          println("Informing results actor")
          resultsActor ! result
        } catch {
          case e : Exception => e.printStackTrace
            println("Informing results actor of exception")
            resultsActor ! Errored(e)
        }
      case Finished() => 
        println("Phase 2 finished")
        classOutputter !? Join()
        errorHandler !? Join()
        println("Informing results actor")
        resultsActor ! Finished()
        exit
    }
  }
    
  val typeProvider = new TypeProvider()
  val macroProvider = new MacroDefinitionProvider()
  val typeResolver = new TypeResolver(typeProvider, macroProvider)
  val callResolver = new CallResolver(macroProvider)
    
  private class Phase1Controller(phase2 : Phase2Controller, errorHandler : ExceptionHandler) extends CompilationController(errorHandler) {
    val intermediates = Buffer[IntermediateResult]()
    
    def handleMessage = {
      case intermediate : IntermediateResult =>
        println("Intermediate: " + intermediate)
        val ast = intermediate.ast
        intermediate.typeNameResolver = new TypeNameResolver(ast, classPathResolver)
        ast.apply(intermediate.lineNumberProvider)

        val finder = new MacroDefinitionFinder(intermediate.lineNumberProvider, typeProvider, macroProvider, intermediate.typeNameResolver)
        ast.apply(finder)
        intermediate.errors ++= finder.errors

        // Output the errors
        for (error <- intermediate.errors) {
          System.out.println(error)
        }
        
        intermediate.ast.apply(typeResolver)
        
        intermediates += intermediate

      case Finished() => 
        println("Starting phase 2")
        macroProvider !? Join()
        typeResolver.stop
        intermediates.foreach(phase2 ! _)
        phase2 ! Finished() 
        exit
    }
  }
  
  private val phase2 = new Phase2Controller(errorHandler)
  phase2.start
  private val phase1 = new Phase1Controller(phase2, errorHandler)
  phase1.start
  
  private def compileAsts(intermediates : Iterable[IntermediateResult]) {
    intermediates.foreach(phase1 ! _)
    phase1 ! Finished()
  }
  

  def compile(units : java.util.List[CompilationUnit]) : java.util.List[CompilationResult] = {
    compile(units, new java.util.LinkedList[String]())
  }
  
  def compile(units : java.util.List[CompilationUnit], classPaths : java.util.List[String]) : java.util.List[CompilationResult] = {
    this.start()

    classPathResolver = new ClassPathResolver(classPaths, this)

    val parserController = new ParserController(this, errorHandler)
    parserController.start
    
    // Parse each source file
    for (unit <- units) {
      parserController ! Parse(unit)
    }
    parserController ! Finished()
    
    debug("Waiting for results actor")
    resultsActor !? Join()

    if (resultsActor.exception != null) {
      throw resultsActor.exception
    }
    
    if (errorHandler.exceptions.size > 0) {
      throw errorHandler.exceptions(0)
    }
    
    debug("Done")
    
    results
  }
}
