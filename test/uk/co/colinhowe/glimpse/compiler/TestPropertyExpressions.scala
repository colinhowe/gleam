package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestPropertyExpressions extends CompilerTest {
  
  @Test
  def compound = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    var compound = c.compound
    node h1 compound.inner
    """ controller new DummyController() compilesTo 
    <view><h1>Inner property</h1></view>
  }
}