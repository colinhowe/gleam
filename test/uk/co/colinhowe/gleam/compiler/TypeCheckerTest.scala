package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.CompilationError
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node
import org.junit.Assert._
import scala.collection.JavaConversions._
import scala.xml._
import uk.co.colinhowe.gleam.TypeCheckError

abstract trait TypeCheckerTest extends CompilerTest {
  
  case class Succeeds(source : String) {
    def succeeds : Unit = {
      val errors = runTypeChecker(new CompilationSet(List(source), null))
      System.out.println(errors)
      assertEquals(0, errors.size)
    }
  }
  
  implicit def succeeds(source : String) = Succeeds(source)
}