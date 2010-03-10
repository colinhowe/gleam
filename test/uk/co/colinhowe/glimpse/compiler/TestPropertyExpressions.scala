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
    node h1 with string
    var compound = c.compound
    h1 compound.inner
    """ controller new DummyController() compilesTo 
    <view><h1>Inner property</h1></view>
  }
}