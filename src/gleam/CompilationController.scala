package gleam

import java.io.BufferedReader
import java.io.PushbackReader
import java.io.StringReader
import java.io.FileOutputStream
import java.io.File

import gleam.compiler.Errored
import gleam.compiler.IntermediateResult
import scala.actors.Actor

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
            if (handler.isDefinedAt(message)) {
              handler.apply(message)
            }
          } catch {
            case _ : scala.util.control.ControlThrowable => // Ignore this
            case e => 
              println("oh noes!")
              exceptionHandler ! Errored(e)
          }
      }
    }
  }
}