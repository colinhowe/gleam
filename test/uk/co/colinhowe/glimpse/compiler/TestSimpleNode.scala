package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestSimpleNode extends CompilerTest {
  
  @Test
  def basic = {   
    """
    node h1 "title"
    """ compilesTo 
    <view><h1>title</h1></view>
  }
  
  @Test
  def compound = {   
    """
    node div {
      node h1 "title"
    }
    """ compilesTo 
    <view>
      <div>
        <h1>title</h1>
      </div>
    </view>
  }
  
  @Test
  def basicWithArguments = {   
    """
    node h1(name: "theName") "title"
    """ compilesTo 
    <view><h1 name="theName">title</h1></view>
  }
  
  @Test
  def compoundWithArguments = {   
    """
    node div(name: "theName") {
      node h1 "title"
    }
    """ compilesTo 
    <view>
      <div name="theName">
        <h1>title</h1>
      </div>
    </view>
  }
  
  @Test
  def multiLineString = {   
    """
    node h1 ""
        title
    ""
    """ compilesTo 
    <view><h1>  title</h1></view>
  }
  
  @Test
  def multiLineStringWithQuotes = {   
    """
    node h1 ""
      "title"
        indented
    ""
    """ compilesTo 
    <view><h1>"title"
  indented</h1></view>
  }
}