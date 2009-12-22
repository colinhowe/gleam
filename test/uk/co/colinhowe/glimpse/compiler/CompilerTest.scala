package uk.co.colinhowe.glimpse.compiler

import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import org.junit.Assert._
import scala.collection.JavaConversions._
import scala.xml._

abstract trait CompilerTest {
  val view_name = "basic_string"
  
  implicit def compilesTo(source : String) = {
    CompilesTo(new CompilationUnit(source, null))
  }
  
  implicit def compilesTo(compilationUnit : CompilationUnit) = {
    CompilesTo(compilationUnit)
  }
  
  case class CompilesTo(compilationUnit : CompilationUnit) {
    def compilesTo(expectedResult : scala.xml.Elem) {
      checkCompilation(compilationUnit, expectedResult)
    }
  }
  
  implicit def controller(source : String) = Controller(source)
  
  case class Controller(source : String) {
    def controller(controller : Object) = new CompilationUnit(source, controller)
  }
  
  case class CompilationUnit(source : String, controller : Object) {
    
  }
 
  def checkCompilation(compilationUnit : CompilationUnit, expectedResult : scala.xml.Elem) {
    new GlimpseCompiler().compile(compilationUnit.source)
    
    // Load the source
    val classesDir = new File("temp/")

    // The parent classloader
    val parentLoader = this.getClass().getClassLoader()

    // Load class "basic_string" with our own classloader.
    val loader1 = new URLClassLoader(
            Array[URL] ( classesDir.toURL() ), parentLoader)
    val cls1 = loader1.loadClass(view_name)
    
    val view = if (compilationUnit.controller != null) {
      cls1.getDeclaredConstructors()(0).newInstance(compilationUnit.controller).asInstanceOf[View]
    } else {
      cls1.newInstance().asInstanceOf[View]
    }
    val nodes = view.getClass().getMethods()(0).invoke(view).asInstanceOf[java.util.List[Node]]

    val scalaNodes = List.fromArray(nodes.toArray).asInstanceOf[List[Node]]
    
    // Print the nodes and check them out
    val xml = 
      "<view>\n" +
      printXml(scalaNodes).split("\n").foldLeft("") { _ + "  " + _ + "\n" } +
      "</view>"
    
    assertEquals(printXml(expectedResult), xml)
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
        val inner = printXml(List.fromArray(node.getNodes().toArray).asInstanceOf[List[Node]])
        inner.split("\n").foldLeft("") { _ + "  " + _ + "\n" }
      } else {
        "  " + node.getText() + "\n"
      }
    "<" + node.getId() + attrs + ">\n" +
    innerText +
    "</" + node.getId() + ">"
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