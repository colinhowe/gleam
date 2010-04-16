package gleam.compiler
import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node

import org.junit.Assert._

class TestImport extends CompilerTest {
  
  @Test
  def specificImport = {   
    """
    import gleam.compiler.DummyController
    controller DummyController
    node h1 with string
    h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def wildCardImport = {   
    """
    import gleam.compiler.*
    controller DummyController
    node h1 with string
    h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def importForMacroParameter = {   
    """
    import java.lang.Object
    
    node p(o : Object) with string
    p(o : "object") "hi"
    """ compilesTo
    <view><p o="object">hi</p></view>
  }
} 