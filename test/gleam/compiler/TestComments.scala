package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._

class TestComments extends CompilerTest {
  
  @Test
  def singleLineComment = {   
    """
    // Single line comment
    node p with string
    p "a"
    """ compilesTo 
    <view><p>a</p></view>
  }
  
  @Test
  def multiLineComment = {   
    """
    /*
     Multi line comment
     */
    node p with string
    p "b"
    """ compilesTo 
    <view><p>b</p></view>
  }
  
  @Test
  def multipleMultiLineComments = {   
    """
    /*
     */
    node p with string
    p "a"
    /*
     */
    p "b"
    """ compilesTo 
    <view><p>a</p><p>b</p></view>
  }
  
  @Test
  def multiLineCommentInMultiLineComment = {   
    """
    /*
     /* The start of this comment is ignored.. so we only end once */
    node p with string
    p "c"
    """ compilesTo 
    <view><p>c</p></view>
  }
  
  @Test
  def partialMultiLineCommentInMultiLineComment = {   
    """
    /*
     /* Inner one!
     */
    node p with string
    p "d"
    """ compilesTo 
    <view><p>d</p></view>
  }
  
  @Test
  def singleLineCommentInMultiLineComment = {   
    """
    /*
     // Inner one!
     */
    node p with string
    p "e"
    """ compilesTo 
    <view><p>e</p></view>
  }
}