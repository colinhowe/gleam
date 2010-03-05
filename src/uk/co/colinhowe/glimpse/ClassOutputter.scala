package uk.co.colinhowe.glimpse
import java.io.FileOutputStream
import java.io.File

import uk.co.colinhowe.glimpse.compiler.Join
import uk.co.colinhowe.glimpse.compiler.Joined
import scala.actors.Actor

case class OutputClass(bytes : Array[Byte], className : String)

class ClassOutputter(outputPath : String) extends Actor {
  def act() {
    loop {
      react {
        // TODO Extract actors out into a new class that captures and passes on errors
        case message : OutputClass => output(message)
        case Join() =>
          reply(Joined)
          exit
      }
    }
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