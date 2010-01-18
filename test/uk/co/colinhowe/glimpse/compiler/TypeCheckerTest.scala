package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.CompilationError
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import org.junit.Assert._
import scala.collection.JavaConversions._
import scala.xml._
import uk.co.colinhowe.glimpse.TypeCheckError

abstract trait TypeCheckerTest extends CompilerTest {
  case class FailsWith(source : String) {
    def failsWith(expectedError : TypeCheckError) : Unit = {
      System.out.println("Expected: " + expectedError)
      val errors = runTypeChecker(new CompilationSet(List(source), null))
      val actualError = errors(0)
      assertEquals(expectedError, actualError)
    }
  }
  
  implicit def failsWith(source : String) = FailsWith(source)
  
  case class Succeeds(source : String) {
    def succeeds : Unit = {
      val errors = runTypeChecker(new CompilationSet(List(source), null))
      System.out.println(errors)
      assertEquals(0, errors.size)
    }
  }
  
  implicit def succeeds(source : String) = Succeeds(source)
  
  def runTypeChecker(compilationUnit : CompilationSet) : List[CompilationError] = {
    val results = compile(compilationUnit)
    var errors = List[CompilationError]()
    for (result <- results) {
      for (error <- result.getErrors()) {
        errors = error.asInstanceOf[CompilationError] :: errors
      }
    }
    errors
  }
}