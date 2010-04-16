package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._

class TestPropertyExpressions extends CompilerTest {
  
  @Test
  def compound = {   
    """
    controller gleam.compiler.DummyController
    node h1 with string
    var compound = c.compound
    h1 compound.inner
    """ controller new DummyController() compilesTo 
    <view><h1>Inner property</h1></view>
  }
}