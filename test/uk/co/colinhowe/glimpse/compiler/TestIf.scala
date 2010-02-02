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

class TestIf extends TypeCheckerTest {
  
  @Test
  def simpleIf = {   
    """
    if (false) {
      node h1 false
    }
    node h1 true
    """ compilesTo 
    <view><h1>true</h1></view>
  }
  
  @Test
  def ifElse = {   
    """
    if (false) {
      node h1 false
    } else {
      node h1 "else"
    }
    node h1 true
    """ compilesTo 
    <view><h1>else</h1><h1>true</h1></view>
  }
  
}