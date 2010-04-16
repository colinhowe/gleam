package gleam.compiler
import org.junit.Test
import gleam.IdentifierNotFoundError

import org.junit.Assert._

class TestDefaultArguments extends TypeCheckerTest {
  
  @Test
  def useDefault = { 
    """
    node field_node(readonly: bool) with string
    macro field(readonly: bool = false) with s : string {
      field_node(readonly: readonly) s
    }
    field "someField"
    """ compilesTo 
    <view><field_node readonly="false">someField</field_node></view>
  }
  
  @Test
  def overrideDefault = { 
    """
    node field_node(readonly: bool) with string
    macro field(readonly: bool = false) with s : string {
      field_node(readonly: readonly) s
    }
    field(readonly: true) "someField"
    """ compilesTo 
    <view><field_node readonly="true">someField</field_node></view>
  }
  
  @Test
  def cascadeOverridesDefault = { 
    """
    node field_node(readonly: bool) with string
    macro fieldset(cascade readonly: bool) with g : generator {
      include g
    }
    macro field(readonly: bool = false) with s : string {
      field_node(readonly: readonly) s
    }
    fieldset(readonly: true) {
      field "someField"
    }
    """ compilesTo 
    <view><field_node readonly="true">someField</field_node></view>
  }
  
  @Test
  def defaultCascaded = { 
    """
    node field_node(readonly: bool) with string
    macro fieldset(cascade readonly: bool = false) with g : generator {
      include g
    }
    macro field(readonly: bool) with s : string {
      field_node(readonly: readonly) s
    }
    fieldset {
      field "someField"
    }
    """ compilesTo 
    <view><field_node readonly="false">someField</field_node></view>
  }
  
  // TODO Think about default expressions that don't type check
  // TODO Think about defaults and dynamic macros
  

}