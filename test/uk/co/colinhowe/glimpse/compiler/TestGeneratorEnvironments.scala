package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import uk.co.colinhowe.glimpse.IdentifierNotFoundError

import org.junit.Assert._

class TestGeneratorEnvironments extends CompilerTest {
  
  @Test
  def basicChanges = {   
    """
    var x = 4
    node div {
      x++
    }
    node p x
    """ compilesTo 
    <view><div /><p>5</p></view>
  }
  
  @Test
  def changesInMacroInvokation = {   
    """
    macro div with g : generator {
      node div {
        include g
      }   
    }
    var x = 4
    div {
      x++
    }
    node p x
    """ compilesTo 
    <view><div /><p>5</p></view>
  }
  
  @Test
  def macroArgumentsSafe = {   
    """
    macro p(value : int) with s : string {
      value++
      node p(value: value) s
    }
    var y = 10
    p(value: y) "hi"
    p(value: y) "hi2"
    """ compilesTo 
    <view><p value="11">hi</p><p value="11">hi2</p></view>
  }
  
  @Test
  def generatorArgumentsSafe = {   
    """
    macro p with g : generator(value : int) {
      var x = 4
      include g(value: x) 
      node p x
    }
    p { value : int =>
      value++
    }
    """ compilesTo 
    <view><p>4</p></view>
  }
  
  @Test
  def singleGeneratorArgument = {   
    """
    macro p with g : generator(value : int) {
      include g(v1: 1) 
    }
    p { v1 : int =>
      node p v1
    }
    """ compilesTo 
    <view><p>1</p></view>
  }
  
  @Test
  def generatorArguments = {   
    """
    macro p with g : generator(value : int) {
      include g(v1: 1, v2: 2) 
    }
    p { v1 : int, v2 : int =>
      node p v1
      node p v2
    }
    """ compilesTo 
    <view><p>1</p><p>2</p></view>
  }
  
  @Test
  def cannotBindOutsideMacroGenerator = {   
    """
    macro p with g : generator {
      node p "hi"
      y++
    }
    var y = 4
    """ errors classOf[IdentifierNotFoundError]
    // TODO Should be a compile failure y cannot be found
  }
}