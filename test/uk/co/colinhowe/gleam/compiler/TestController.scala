package uk.co.colinhowe.gleam.compiler
import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node

import org.junit.Assert._

class TestController extends CompilerTest {
  
  @Test
  def useController = {   
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    node h1 with string
    h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def compoundProperties = {   
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    node h1 with string
    h1 c.compound.inner
    """ controller(new DummyController) compilesTo
    <view><h1>Inner property</h1></view>
  }
} 