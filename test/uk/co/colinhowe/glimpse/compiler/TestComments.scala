package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestComments extends CompilerTest {
  
  @Test
  def singleLineComment = {   
    """
    // Single line comment
    node:p "a"
    """ compilesTo 
    <view><p>a</p></view>
  }
  
  @Test
  def multiLineComment = {   
    """
    /*
     Multi line comment
     */
    node:p "b"
    """ compilesTo 
    <view><p>b</p></view>
  }
  
  @Test
  def multiLineCommentInMultiLineComment = {   
    """
    /*
     /* Inner one! */
     */
    node:p "c"
    """ compilesTo 
    <view><p>c</p></view>
  }
  
  @Test
  def partialMultiLineCommentInMultiLineComment = {   
    """
    /*
     /* Inner one!
     */
    node:p "d"
    """ compilesTo 
    <view><p>d</p></view>
  }
  
  @Test
  def singleLineCommentInMultiLineComment = {   
    """
    /*
     // Inner one!
     */
    node:p "e"
    """ compilesTo 
    <view><p>e</p></view>
  }
}