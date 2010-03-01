package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestMacro extends CompilerTest {
  
  @Test
  def generator = {   
    """
    macro div with g : generator {
      node div {
        include g
      }   
    }
    div {
      node p "Inside"
    }
    """ compilesTo <view>
      <div>
        <p>Inside</p>
      </div>
    </view>
  }
  
  @Test
  def generatorInMacroStmt = {   
    """
    macro panel with g : generator {
      div {
        include g
      }
    }
    macro div with g : generator {
      node div {
        include g
      }   
    }
    panel {
      node p "Inside"
    }
    """ compilesTo <view>
      <div>
        <p>Inside</p>
      </div>
    </view>
  }
  
  @Test
  def string = {   
    """
    macro p with s : string {
      node p s
    }
    p "Hi"
    """ compilesTo <view>
      <p>Hi</p>
    </view>
  }
  
  @Test
  def macroWithStringArguments = {   
    """
    macro p(value : int) with s : string {
      node p(value: value) s
    }
    p(value: 4) "hi"
    """ compilesTo 
    <view><p value="4">hi</p></view>
  }
  
  @Test
  def macroWithGeneratorArguments = {   
    """
    macro div(value : int) with g : generator {
      node div(value: value) {
        include g
      }
    }
    div(value: 4) {
      node span "hi"
    }
    """ compilesTo 
    <view><div value="4"><span>hi</span></div></view>
  }
  
  @Test
  def macroCallWithPropertyAsValue = {   
    """
    macro p(value : int) with s : string {
      node p(value: value) s
    }
    var hiString = "hi"
    p(value: 4) hiString
    """ compilesTo 
    <view><p value="4">hi</p></view>
  }
  
  @Test
  def macroWithOverloadedValue = {   
    """
    macro p with s : string {
      node p s
    }
    macro p with g : generator {
      node p {
        include g
      }
    }
    p "string"
    p {
      p "generator"
    }
    """ compilesTo 
    <view><p>string</p><p><p>generator</p></p></view>
  }
  
  @Test
  def macroWithNoValue = {   
    """
    macro br with s : string {
      node br s
    }
    br
    br
    """ compilesTo 
    <view><br /><br /></view>
  }
}