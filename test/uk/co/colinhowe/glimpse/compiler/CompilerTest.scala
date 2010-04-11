package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.CompilationError


import uk.co.colinhowe.glimpse.CompilationResult

import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import org.junit.Assert._
import scala.collection.JavaConversions._
import scala.xml._

import org.junit.After

abstract trait CompilerTest {
  val view_name = "basic_string"
  
  @After
  def clearTempFiles = {
    val tempFolder = new File("temp")
    for (file <- tempFolder.listFiles) {
      file.delete
    }
  }
  
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
  
  case class FailsWith(compilationSet : CompilationSet) {
    def failsWith(expectedError : CompilationError) : Unit = {
      System.out.println("Expected: " + expectedError)
      val errors = runTypeChecker(compilationSet)
      val actualError = errors(0)
      assertEquals(expectedError, actualError)
    }
    
    def failsWith(expectedErrors : List[CompilationError]) : Unit = {
      System.out.println("Expected: " + expectedErrors)
      val actualErrors = runTypeChecker(compilationSet)
      assertEquals(expectedErrors, actualErrors)
    }
  }
  
  implicit def failsWith(source : String) = FailsWith(new CompilationSet(List(source), null))
  implicit def failsWith(compilationSet : CompilationSet) = FailsWith(compilationSet)
    
  implicit def errors(source : String) = {
    Errors(new CompilationSet(List(source), null))
  }
  
  case class Errors(CompilationSet : CompilationSet) {
    def errors(expectedError : java.lang.Class[_]) {
      val results = compile(CompilationSet)
      
      for (result <- results) {
        for (error <- result.getErrors()) {
          if (error.getClass.equals(expectedError)) {
            return
          }
        }
      }
      fail("Did not find error [" + expectedError + "]")
    }
  }
    
  implicit def compilationSet(source : String) = {
    new CompilationSet(List(source), null)
  }
  
  class CompilationSet(val sources : List[String], val controller : Object) {
    def and(source : String) : CompilationSet = {
      new CompilationSet(source :: sources, controller)
    }

    def controller(controller : Object) = new CompilationSet(sources, controller)

    def compilesTo(expectedResult : scala.xml.Elem) {
      checkCompilation(this, expectedResult)
    }
  }
  
  def compile(compilationSet : CompilationSet) : scala.collection.mutable.Buffer[CompilationResult] = {
    val tempFolder = new File("temp/")
    if (!tempFolder.exists) {
      tempFolder.mkdir()
      tempFolder.deleteOnExit
    }
    
    // Build up a list of compilation units
    val compilationUnits = scala.collection.mutable.ListBuffer[CompilationUnit]()
    var i = 0
    for (source <- compilationSet.sources) {
      val compilationUnit = new StringCompilationUnit("view" + i, "nosource", source)
      i = i + 1
      compilationUnits += compilationUnit
    }
    
    return new GlimpseCompiler().compile(compilationUnits, scala.collection.mutable.Buffer[String]("bin"))
  }
  
  def checkCompilation(compilationSet : CompilationSet, expectedResult : scala.xml.Elem) {
    val results = compile(compilationSet)
    
    var errors = List[CompilationError]()
    
    for (result <- results) {
      for (error <- result.getErrors()) {
        errors = error.asInstanceOf[CompilationError] :: errors
      }
    }
    if (errors.size > 0) {
      assertEquals("Got errors [" + errors + "]", 0, errors.size)
    }
    
    // Load the source
    val classesDir = new File("temp/")

    // The parent classloader
    val parentLoader = this.getClass().getClassLoader()

    // Load class "basic_string" with our own classloader.
    val loader1 = new URLClassLoader(
            Array[URL] ( classesDir.toURL() ), parentLoader)
    
    val cls1 = loader1.loadClass("view0")
    
    val view = cls1.newInstance().asInstanceOf[View]
    val invokeMethod = view.getClass().getMethods()(0)
    val nodes = invokeMethod.invoke(view, compilationSet.controller).asInstanceOf[java.util.List[Node]]
    
    val scalaNodes : List[Node] = nodes.toList
    
    // Print the nodes and check them out
    val xml = 
      "<view>\n" +
      printXml(scalaNodes).split("\n").foldLeft("") { _ + "  " + _ + "\n" } +
      "</view>"
    
    assertEquals(printXml(expectedResult).replaceAll("\r", ""), xml.replaceAll("\r", ""))
  }
 
  def printXml(nodes : List[Node]) : String = {
    nodes.foldLeft("") { (acc, node) =>
      acc + printXml(node) + "\n"
    }
  }
  
  def printXml(node : Node) : String = {
    val attrs = 
      if (node.getAttributes.size() > 0) {
        " " + node.getAttributes.keySet().map { k =>
          k+"=\""+node.getAttributes.get(k)+"\""
        }.mkString("", " ", "")
      } else {
        ""
      }
    val innerText =
      if (node.getNodes() != null) {
        val inner = printXml(node.getNodes().toList)
        inner.split("\n").foldLeft("") { _ + "  " + _ + "\n" }
      } else if (node.getValue() != null) {
        "  " + node.getValue() + "\n"
      } else {
        "  \n"
      }
    "<" + node.getTagName() + attrs + ">\n" +
    innerText +
    "</" + node.getTagName() + ">"
  }
 
  
  
  def printXml(nodes : Seq[scala.xml.Node]) : String = {
    nodes
      .map { printXml(_) }
      .filter { _ != "" }
      .foldLeft("") { (acc, node) => acc + node + "\n" }
  }
  
  def printXml(node : scala.xml.Node) : String = {
    if (node.label == "#PCDATA") {
      return ""
    }
    val attrs = 
      if (node.attributes.length > 0) {
        " " + node.attributes.map { m : MetaData =>
           m.key+"=\""+m.value+"\""
        }.mkString("", " ", "")
      } else {
        ""
      }
    val innerText =
      if (node.child.filter { _.label != "#PCDATA" }.length > 0) {
        val inner = printXml(node.child)
        inner.split("\n").foldLeft("") { _ + "  " + _ + "\n" }
      } else {
        "  " + node.child.text + "\n"
      }

    "<" + node.label + attrs + ">\n" +
    innerText +
    "</" + node.label + ">"
  }
}
