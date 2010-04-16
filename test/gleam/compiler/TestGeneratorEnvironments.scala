package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node
import gleam.IdentifierNotFoundError

import org.junit.Assert._

class TestGeneratorEnvironments extends CompilerTest {
  
  @Test
  def basicChanges = {   
    """
    node div with generator
    node p with int
    var x = 4
    div {
      x++
    }
    p x
    """ compilesTo 
    <view><div /><p>5</p></view>
  }
  
  @Test
  def changesInMacroInvokation = {   
    """
    node div_node with generator
    node p with int
    macro div with g : generator {
      div_node {
        include g
      }   
    }
    var x = 4
    div {
      x++
    }
    p x
    """ compilesTo 
    <view><div_node /><p>5</p></view>
  }
  
  @Test
  def macroArgumentsSafe = {   
    """
    node p_node(value : int) with string
    macro p(value : int) with s : string {
      value++
      p_node(value: value) s
    }
    var y = 10
    p(value: y) "hi"
    p(value: y) "hi2"
    """ compilesTo 
    <view><p_node value="11">hi</p_node><p_node value="11">hi2</p_node></view>
  }
  
  @Test
  def generatorArgumentsSafe = {   
    """
    node p_node with int
    macro p with g : generator(value : int) {
      var x = 4
      include g(value: x) 
      p_node x
    }
    p { value : int =>
      value++
    }
    """ compilesTo 
    <view><p_node>4</p_node></view>
  }
  
  @Test
  def singleGeneratorArgument = {   
    """
    node p with int
    macro p with g : generator(value : int) {
      include g(v1: 1) 
    }
    p { v1 : int =>
      p v1
    }
    """ compilesTo 
    <view><p>1</p></view>
  }
  
  @Test
  def generatorArguments = {   
    """
    node p with int
    macro p with g : generator(value : int) {
      include g(v1: 1, v2: 2) 
    }
    p { v1 : int, v2 : int =>
      p v1
      p v2
    }
    """ compilesTo 
    <view><p>1</p><p>2</p></view>
  }
  
  @Test
  def cannotBindOutsideMacroGenerator = {   
    """
    macro p with g : generator {
      y++
    }
    var y = 4
    """ errors classOf[IdentifierNotFoundError]
    // TODO Should be a compile failure y cannot be found
  }
  
  @Test
  def variableHiding = {   
    """
    node div with generator
    node p with int
    var x = 4
    div {
      var x = 2
      p x
    }
    p x
    """ compilesTo 
    <view><div><p>2</p></div><p>4</p></view>
  }
}