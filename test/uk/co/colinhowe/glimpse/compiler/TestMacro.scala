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
    node d with generator
    node p with string
    macro div with g : generator {
      d {
        include g
      }   
    }
    div {
      p "Inside"
    }
    """ compilesTo <view>
      <d>
        <p>Inside</p>
      </d>
    </view>
  }
  
  @Test
  def generatorInMacroStmt = {   
    """
    node d with generator
    node p with string
    macro panel with g : generator {
      div {
        include g
      }
    }
    macro div with g : generator {
      d {
        include g
      }   
    }
    panel {
      p "Inside"
    }
    """ compilesTo <view>
      <d>
        <p>Inside</p>
      </d>
    </view>
  }
  
  @Test
  def string = {   
    """
    node x with string
    macro p with s : string {
      x s
    }
    p "Hi"
    """ compilesTo <view>
      <x>Hi</x>
    </view>
  }
  
  @Test
  def macroWithStringArguments = {   
    """
    node x(value : int) with string
    macro p(value : int) with s : string {
      x(value: value) s
    }
    p(value: 4) "hi"
    """ compilesTo 
    <view><x value="4">hi</x></view>
  }
  
  @Test
  def macroWithGeneratorArguments = {   
    """
    node d(value : int) with generator
    node span with string
    macro div(value : int) with g : generator {
      d(value: value) {
        include g
      }
    }
    div(value: 4) {
      span "hi"
    }
    """ compilesTo 
    <view><d value="4"><span>hi</span></d></view>
  }
  
  @Test
  def macroCallWithPropertyAsValue = {   
    """
    node x(value: int) with string
    macro p(value : int) with s : string {
      x(value: value) s
    }
    var hiString = "hi"
    p(value: 4) hiString
    """ compilesTo 
    <view><x value="4">hi</x></view>
  }
  
  @Test
  def macroWithOverloadedValue = {   
    """
    node x with string
    node x with generator
    macro p with s : string {
      x s
    }
    macro p with g : generator {
      x {
        include g
      }
    }
    p "string"
    p {
      p "generator"
    }
    """ compilesTo 
    <view><x>string</x><x><x>generator</x></x></view>
  }
  
  @Test
  def macroWithNoValue = {   
    """
    node t
    macro br {
      t
    }
    br
    br
    """ compilesTo 
    <view><t /><t /></view>
  }
}