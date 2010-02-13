package uk.co.colinhowe.glimpse.compiler
import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestImport extends CompilerTest {
  
  @Test
  def specificImport = {   
    """
    import uk.co.colinhowe.glimpse.compiler.DummyController
    controller DummyController
    node h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def wildCardImport = {   
    """
    import uk.co.colinhowe.glimpse.compiler.*
    controller DummyController
    node h1 c.name
    """ controller(new DummyController) compilesTo
    <view><h1>Name of the controller</h1></view>
  }
  
  @Test
  def importForMacroParameter = {   
    """
    import java.lang.Object
    
    macro p(o : Object) with s : string {
      node p s
    }
    p(o : "object") "hi"
    """ compilesTo
    <view><p>hi</p></view>
  }
} 