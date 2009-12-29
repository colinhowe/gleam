package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestExpressions extends CompilerTest {
  
  @Test
  def intDeclaration = {   
    """    
    int x = 1
    node:h1 x
    """ compilesTo 
    <view><h1>1</h1></view>
  }
  
  @Test
  def increment = { 
    """
    int x = 1
    x++
    node:h1 x
    """ compilesTo 
    <view><h1>2</h1></view>
  }
  
  @Test
  def variableReassignment = {   
    """    
    int x = 1
    x = 4
    node:h1 x
    """ compilesTo 
    <view><h1>4</h1></view>
  }
}