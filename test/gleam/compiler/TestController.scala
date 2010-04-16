package gleam.compiler
import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._

class TestController extends CompilerTest {
  
  @Test
  def useController = {   
    """
    controller gleam.compiler.DummyController
    node h1 with string
    h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def compoundProperties = {   
    """
    controller gleam.compiler.DummyController
    node h1 with string
    h1 c.compound.inner
    """ controller(new DummyController) compilesTo
    <view><h1>Inner property</h1></view>
  }
} 