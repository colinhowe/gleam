package uk.co.colinhowe.glimpse

import java.io.BufferedReader
import java.io.PushbackReader
import java.io.StringReader
import java.io.FileOutputStream
import java.io.File

import uk.co.colinhowe.glimpse.compiler.Errored
import uk.co.colinhowe.glimpse.compiler.IntermediateResult
import scala.actors.Actor
import scala.util.control.ControlException

abstract class CompilationController(exceptionHandler : ExceptionHandler) extends Actor {
  def handleMessage : PartialFunction[Any, Unit]

  def defaultHandler : PartialFunction[Any, Unit] = { 
    case m => throw new RuntimeException("Unhandled message [" + m + "]") 
  }
  
  def handler = handleMessage.orElse(defaultHandler)
  
  def act() {
    loop {
      react {
        case message =>
          try {
            handler.apply(message)
          } catch {
            case _ : ControlException => 
            case e => 
              println("oh noes!")
              exceptionHandler ! Errored(e)
          }
      }
    }
  }
}