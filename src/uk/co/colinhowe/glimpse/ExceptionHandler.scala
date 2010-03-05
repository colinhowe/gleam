package uk.co.colinhowe.glimpse

import uk.co.colinhowe.glimpse.compiler.CompilationUnit
import uk.co.colinhowe.glimpse.compiler.Errored

import scala.actors.Actor
import scala.collection.mutable.Buffer

class ExceptionHandler extends Actor {
  val exceptions = Buffer[Throwable]()
  
  def act() {
    loop {
      react {
        case Errored(e) => exceptions += e
      }
    }
  }
}