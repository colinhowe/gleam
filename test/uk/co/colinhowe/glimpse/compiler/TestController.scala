package uk.co.colinhowe.glimpse.compiler
import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestController extends CompilerTest {
  
  @Test
  def useController = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node:h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def compoundProperties = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node:h1 c.compound.inner
    """ controller(new DummyController) compilesTo
    <view><h1>Inner property</h1></view>
  }
} 