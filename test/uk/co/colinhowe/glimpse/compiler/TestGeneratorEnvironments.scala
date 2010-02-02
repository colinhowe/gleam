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
    int x = 4
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
    macro div with generator g {
      node div {
        include g
      }   
    }
    int x = 4
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
    macro p(int value) with string s {
      value++
      node p(value: value) s
    }
    int y = 10
    p(value: y) "hi"
    p(value: y) "hi2"
    """ compilesTo 
    <view><p value="11">hi</p><p value="11">hi2</p></view>
  }
  
  @Test
  def generatorArgumentsSafe = {   
    """
    macro p with generator g(int value) {
      int x = 4
      include g(value: x) 
      node p x
    }
    p { int value =>
      value++
    }
    """ compilesTo 
    <view><p>4</p></view>
  }
  
  @Test
  def singleGeneratorArgument = {   
    """
    macro p with generator g(int value) {
      include g(v1: 1) 
    }
    p { int v1 =>
      node p v1
    }
    """ compilesTo 
    <view><p>1</p></view>
  }
  
  // TODO This should error!
  @Test
  def generatorArguments = {   
    """
    macro p with generator g(int value) {
      include g(v1: 1, v2: 2) 
    }
    p { int v1, int v2 =>
      node p v1
      node p v2
    }
    """ compilesTo 
    <view><p>1</p><p>2</p></view>
  }
  
  @Test
  def cannotBindOutsideMacroGenerator = {   
    """
    macro p with generator g {
      node p "hi"
      y++
    }
    int y = 4
    """ errors classOf[IdentifierNotFoundError]
    // TODO Should be a compile failure y cannot be found
  }
}