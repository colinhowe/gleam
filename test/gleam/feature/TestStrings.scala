package gleam.feature

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._
import gleam.compiler.CompilerTest

class TestStrings extends CompilerTest {
  @Test
  def multiLineString = {   
    """
    node h1 with string
    h1 "
        title
    "
    """ compilesTo 
    <view><h1>  title</h1></view>
  }
  
  @Test
  def singleLineString = {   
    """
    node h1 with string
    h1 "title"
    """ compilesTo 
    <view><h1>title</h1></view>
  }
  
  @Test
  def emptyString = {   
    """
    node h1 with string
    h1 ""
    """ compilesTo 
    <view><h1></h1></view>
  }
  
  @Test
  def twoMultiLineStrings = {
    """
    node h1 with string
    h1 "
      title
    "
    h1 "
      title2
    "
    """ compilesTo 
    <view><h1>title</h1><h1>title2</h1></view>
  }
  
  @Test
  def multiLineStringWithQuotes = {   
    """
    node h1 with string
    h1 "
      \"title\"
        indented
    "
    """ compilesTo 
    <view><h1>"title"
  indented</h1></view>
  }
}