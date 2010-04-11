package uk.co.colinhowe.gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node

import org.junit.Assert._

class TestExpressions extends CompilerTest {
  
  @Test
  def intDeclaration = {   
    """    
    node h1 with int
    var x = 1
    h1 x
    """ compilesTo 
    <view><h1>1</h1></view>
  }
  
  @Test
  def increment = { 
    """
    node h1 with int
    var x = 1
    x++
    h1 x
    """ compilesTo 
    <view><h1>2</h1></view>
  }
  
  @Test
  def variableReassignment = {   
    """    
    node h1 with int
    var x = 1
    x = 4
    h1 x
    """ compilesTo 
    <view><h1>4</h1></view>
  }
}