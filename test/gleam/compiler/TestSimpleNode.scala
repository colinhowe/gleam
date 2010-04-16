package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._

class TestSimpleNode extends CompilerTest {
  
  @Test
  def basic = {   
    """
    node h1 with string

    h1 "title"
    """ compilesTo 
    <view><h1>title</h1></view>
  }
  
  @Test
  def withDefaults = {   
    """
    node h1(class : string = "someClass") with string

    h1 "title"
    """ compilesTo 
    <view><h1 class="someClass">title</h1></view>
  }
  
  @Test
  def cascade = {   
    """
    node field_group(cascade readonly : bool) with generator
    node field(readonly : bool) with string

    field_group(readonly: true) {
      field "hi"
    }
    """ compilesTo 
    <view><field_group readonly="true"><field readonly="true">hi</field></field_group></view>
  }
  
  @Test
  def cascadeAndDefault = {   
    """
    node field_group(cascade readonly : bool = false) with generator
    node field(readonly : bool) with string

    field_group {
      field "hi"
    }
    """ compilesTo 
    <view><field_group readonly="false"><field readonly="false">hi</field></field_group></view>
  }
  
  @Test
  def compound = {   
    """
    node div with generator
    node h1 with string

    div {
      h1 "title"
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
    node h1(name: string) with string

    h1(name: "theName") "title"
    """ compilesTo 
    <view><h1 name="theName">title</h1></view>
  }
  
  @Test
  def compoundWithArguments = {   
    """
    node div(name: string) with generator
    node h1 with string

    div(name: "theName") {
      h1 "title"
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
    node h1 with string
    h1 ""
        title
    ""
    """ compilesTo 
    <view><h1>  title</h1></view>
  }
  
  @Test
  def twoMultiLineStrings = {   
    """
    node h1 with string
    h1 ""
      title
    ""
    h1 ""
      title2
    ""
    """ compilesTo 
    <view><h1>title</h1><h1>title2</h1></view>
  }
  
  @Test
  def multiLineStringWithQuotes = {   
    """
    node h1 with string
    h1 ""
      "title"
        indented
    ""
    """ compilesTo 
    <view><h1>"title"
  indented</h1></view>
  }
}