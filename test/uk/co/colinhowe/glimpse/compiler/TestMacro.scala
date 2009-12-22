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
    macro div with generator g {
      node:div {
        include g
      }   
    }
    div {
      node:p "Inside"
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
    macro p with string s {
      node:p s
    }
    p "Hi"
    """ compilesTo <view>
      <p>Hi</p>
    </view>
  }
}