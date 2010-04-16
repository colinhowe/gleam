package gleam
import java.io.FileOutputStream
import java.io.File

import gleam.compiler.Join
import gleam.compiler.Joined
import scala.actors.Actor

case class OutputClass(bytes : Array[Byte], className : String)

class ClassOutputter(outputPath : String, exceptionHandler : ExceptionHandler) extends CompilationController(exceptionHandler) {
  def handleMessage = {
    // TODO Extract actors out into a new class that captures and passes on errors
    case message : OutputClass => output(message)
    case Join() =>
      reply(Joined)
      exit
  }
  
  def output(message : OutputClass) {
    val stream = new FileOutputStream(new File(outputPath + "/" + message.className + ".class"))
    try {
      stream.write(message.bytes)
    } finally {
      stream.close()
    }
  }
}