package gleam.compiler
import gleam.compiler.typing.SimpleType

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node
import gleam.TypeCheckError

import org.junit.Assert._

class TestIf extends TypeCheckerTest {
  
  @Test
  def simpleIf = {   
    """
    node h1 with bool
    if (false) {
      h1 false
    }
    h1 true
    """ compilesTo 
    <view><h1>true</h1></view>
  }
  
  @Test
  def ifElseWithIfFalse = {   
    """
    node h1 with java.lang.Object
    if (false) {
      h1 false
    } else {
      h1 "else"
    }
    h1 true
    """ compilesTo 
    <view><h1>else</h1><h1>true</h1></view>
  }
  
  @Test
  def ifElseWithIfTrue = {   
    """
    node h1 with java.lang.Object
    if (true) {
      h1 "true"
    } else {
      h1 "else"
    }
    h1 true
    """ compilesTo 
    <view><h1>true</h1><h1>true</h1></view>
  }
  
  @Test
  def ifWithVariable = {   
    """
    node h1 with java.lang.Object
    var even = false
    if (even) {
      h1 false
    } else {
      h1 "else"
    }
    h1 true
    """ compilesTo 
    <view><h1>else</h1><h1>true</h1></view>
  }
}