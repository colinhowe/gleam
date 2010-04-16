package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

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