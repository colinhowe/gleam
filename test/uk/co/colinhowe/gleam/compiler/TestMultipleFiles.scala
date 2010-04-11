package uk.co.colinhowe.gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node

import org.junit.Assert._

class TestMultipleFiles extends CompilerTest {
  
  @Test
  def twoFiles = {   
    """
    node div with generator
    """ and """
    node p with string
    div {
      p "Inside"
    }
    """ compilesTo <view>
      <div>
        <p>Inside</p>
      </div>
    </view>
  }
}