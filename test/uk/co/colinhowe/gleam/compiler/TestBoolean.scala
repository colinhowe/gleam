package uk.co.colinhowe.gleam.compiler
import uk.co.colinhowe.gleam.CompilationError

import uk.co.colinhowe.gleam.compiler.typing.SimpleType

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node
import uk.co.colinhowe.gleam.TypeCheckError

import org.junit.Assert._

class TestBoolean extends TypeCheckerTest {
  
  @Test
  def booleanDeclaration = {   
    """    
    node h1 with bool    var x = false
    h1 x
    """ compilesTo 
    <view><h1>false</h1></view>
  }
  
  @Test
  def inversion = { 
    """
    node h1 with bool    var x = false
    x = !x
    h1 x
    x = !x
    h1 x
    """ compilesTo 
    <view><h1>true</h1><h1>false</h1></view>
  }
  
  @Test
  def inversionOnBadType = { 
    """
    var x = "false"
    x = !x
    """ failsWith
    List[CompilationError](
      TypeCheckError(
          line = 3,
          expectedType = new SimpleType(classOf[String]), 
          actualType = new SimpleType(classOf[java.lang.Boolean])),
      TypeCheckError(
          line = 3,
          expectedType = new SimpleType(classOf[java.lang.Boolean]), 
          actualType = new SimpleType(classOf[String]))
    )          
  }
  
  @Test
  def asMacroArgument = {   
    """    
    node p(value : bool) with string    macro b(value : bool) with s : string {
      p(value: value) s
    }
    b(value: false) "hi"
    """ compilesTo 
    <view><p value="false">hi</p></view>
  }
}