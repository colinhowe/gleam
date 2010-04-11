package uk.co.colinhowe.gleam

import uk.co.colinhowe.gleam.compiler.CompilationUnit
import uk.co.colinhowe.gleam.compiler.Errored

import scala.actors.Actor
import scala.collection.mutable.Buffer
import uk.co.colinhowe.gleam.compiler.Join
import uk.co.colinhowe.gleam.compiler.Joined

case class CompilationException(error : CompilationError) extends Exception

class ExceptionHandler extends Actor {
  val exceptions = Buffer[Throwable]()
  val errors = Buffer[CompilationError]()
  
  def act() {
    loop {
      react {
        case Errored(e) => 
          e.printStackTrace
          exceptions += e
        case CompilationException(error) =>
          System.err.println(error)
          errors += error
        case t : Throwable =>
          exceptions += t
        case Join() =>
          reply(Joined)
          exit
      }
    }
  }
}