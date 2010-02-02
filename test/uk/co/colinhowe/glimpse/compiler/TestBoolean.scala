package uk.co.colinhowe.glimpse.compiler
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
    bool x = false
    node:h1 x
    """ compilesTo 
    <view><h1>false</h1></view>
  }
  
  @Test
  def inversion = { 
    """
    bool x = false
    x = !x
    node:h1 x
    x = !x
    node:h1 x
    """ compilesTo 
    <view><h1>true</h1><h1>false</h1></view>
  }
  
  @Test
  def asMacroArgument = {   
    """    
    macro b(bool value) with string s {
      node:b(value: value) s
    }
    b(value: false) "hi"
    """ compilesTo 
    <view><b value="false">hi</b></view>
  }
  
  @Test
  def typeCheckFailure = { 
    """
    bool x = 1
    """ failsWith
    TypeCheckError(
        line = 2, 
        expectedType = new SimpleType(classOf[java.lang.Boolean]), 
        actualType = new SimpleType(classOf[Integer]))
  }
}