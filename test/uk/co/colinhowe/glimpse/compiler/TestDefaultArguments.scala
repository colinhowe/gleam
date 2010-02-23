package uk.co.colinhowe.glimpse.compiler
import org.junit.Test
import uk.co.colinhowe.glimpse.IdentifierNotFoundError

import org.junit.Assert._

class TestDefaultArguments extends TypeCheckerTest {
  
  @Test
  def useDefault = { 
    """
    macro field(readonly: bool = false) with s : string {
      node field(readonly: readonly) s
    }
    field "someField"
    """ compilesTo 
    <view><field readonly="false">someField</field></view>
  }
  
  @Test
  def overrideDefault = { 
    """
    macro field(readonly: bool = false) with s : string {
      node field(readonly: readonly) s
    }
    field(readonly: true) "someField"
    """ compilesTo 
    <view><field readonly="true">someField</field></view>
  }
  
  @Test
  def cascadeOverridesDefault = { 
    """
    macro fieldset(cascade readonly: bool) with g : generator {
      include g
    }
    macro field(readonly: bool = false) with s : string {
      node field(readonly: readonly) s
    }
    fieldset(readonly: true) {
      field "someField"
    }
    """ compilesTo 
    <view><field readonly="true">someField</field></view>
  }
  
  @Test
  def defaultCascaded = { 
    """
    macro fieldset(cascade readonly: bool = false) with g : generator {
      include g
    }
    macro field(readonly: bool) with s : string {
      node field(readonly: readonly) s
    }
    fieldset {
      field "someField"
    }
    """ compilesTo 
    <view><field readonly="false">someField</field></view>
  }
  
  // TODO Think about default expressions that don't type check
  // TODO Think about defaults and dynamic macros
  

}