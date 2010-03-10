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
    node x(name : string) with string
    macro p(name : string) with s : string {
      x(name: name) s
    }
    p(name: "div1") "hi"
    """ compilesTo <view>
      <x name="div1">hi</x>
    </view>
  }
}