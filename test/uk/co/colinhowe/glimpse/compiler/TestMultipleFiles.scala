package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestMultipleFiles extends CompilerTest {
  
  @Test
  def twoFiles = {   
    """
    macro div with generator g {
      node:div {
        include g
      }   
    }
    """ and """
    div {
      node:p "Inside"
    }
    """ compilesTo <view>
      <div>
        <p>Inside</p>
      </div>
    </view>
  }
}