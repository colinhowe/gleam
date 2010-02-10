package uk.co.colinhowe.glimpse.compiler
import uk.co.colinhowe.glimpse.CompilationError

import uk.co.colinhowe.glimpse.compiler.typing.SimpleType

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import uk.co.colinhowe.glimpse.TypeCheckError

import org.junit.Assert._

class TestBoolean extends TypeCheckerTest {
  
  @Test
  def booleanDeclaration = {   
    """    
    var x = false
    node h1 x
    """ compilesTo 
    <view><h1>false</h1></view>
  }
  
  @Test
  def inversion = { 
    """
    var x = false
    x = !x
    node h1 x
    x = !x
    node h1 x
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
    macro b(value : bool) with s : string {
      node b(value: value) s
    }
    b(value: false) "hi"
    """ compilesTo 
    <view><b value="false">hi</b></view>
  }
}