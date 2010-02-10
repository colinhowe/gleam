package uk.co.colinhowe.glimpse.compiler

import org.junit.Test

class TestPropertyReference extends CompilerTest {
  
  @Test
  def simpleReference = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    
    macro field(p : ref) with s : string {
      node span p.path 
      node span p.value
    }
    
    field(p: @c.name) "ignored"
    """ controller(new DummyController) compilesTo
    <view><span>name</span><span>Name of the controller</span></view>
  }
}