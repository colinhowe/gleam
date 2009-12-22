package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestMultipleArgumentMacro extends CompilerTest {
  
  @Test
  def nameArgument = {   
    """
    macro p(string name) with string s {
      node:p(name: name) s
    }
    p(name: "div1") "hi"
    """ compilesTo <view>
      <p name="div1">hi</p>
    </view>
  }
}