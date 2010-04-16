package gleam

import gleam.compiler.CompilationUnit
import gleam.compiler.Errored

import scala.actors.Actor
import scala.collection.mutable.Buffer
import gleam.compiler.Join
import gleam.compiler.Joined

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