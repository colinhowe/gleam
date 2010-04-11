package uk.co.colinhowe.gleam.compiler

import org.junit.Test

class TestPropertyReference extends CompilerTest {
  
  @Test
  def simpleReference = {   
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    
    node span with java.lang.Object
    macro field(p : ref) with s : string {
      span p.path 
      span p.value
    }
    
    field(p: @c.name) "ignored"
    """ controller(new DummyController) compilesTo
    <view><span>name</span><span>Name of the controller</span></view>
  }
}