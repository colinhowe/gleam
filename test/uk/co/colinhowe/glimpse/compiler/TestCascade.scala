package uk.co.colinhowe.glimpse.compiler
import org.junit.Test
import uk.co.colinhowe.glimpse.IdentifierNotFoundError

import org.junit.Assert._

class TestCascade extends TypeCheckerTest {
  
  @Test
  def immediateCalls = { 
    """
    macro field(readonly: bool) with s : string {
      node field(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      field "someField"
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><field readonly="true">someField</field></view>
  }
  
  
  @Test
  def insideNodeGenerators = { 
    """
    macro field(readonly: bool) with s : string {
      node field(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      node div {
        field "someField"
      }
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><div><field readonly="true">someField</field></div></view>
  }
  
  
  @Test
  def insideMacroGenerators = { 
    """
    macro div with g : generator {
      node div {
        include g
      }
    }
    macro field(readonly: bool) with s : string {
      node field(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      div {
        field "someField"
      }
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><div><field readonly="true">someField</field></div></view>
  }
  
  
  @Test
  def insideIncludes = { 
    """
    macro div with g : generator {
      node div {
        include g
      }
    }
    macro field(readonly: bool) with s : string {
      node field(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with g : generator {
      div {
        include g
      }
    }
    fieldset(readonly: true) {
      field "hi"
    }
    """ compilesTo 
    <view><div><field readonly="true">hi</field></div></view>
  }
  
  
  @Test
  def noCascadeIntoOtherMacros = { 
    """
    macro div with g : generator {
      node x(readonly: readonly) "x"
    }
    macro fieldset(cascade readonly: bool) with s : string {
      div {
        node p "fail"
      }
    }
    fieldset(readonly: true) "fail"
    """ failsWith
    IdentifierNotFoundError(
      line = 3,
      identifier = "readonly"
    )
  }
  
  
  @Test
  def innerVariableSameNameButNoCascade = { 
    """
    macro div(readonly : bool) with g : generator {
      node div {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      node field(readonly: readonly) s
    }
    fieldset(readonly: true) {
      div(readonly: false) {
        field "name"
      }
    }
    """ compilesTo 
    <view><div><field readonly="true">name</field></div></view>
  }
  
  
  @Test
  def innerVariableSameNameAndCascade = { 
    """
    macro div(cascade readonly : bool) with g : generator {
      node div {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      node field(readonly: readonly) s
    }
    fieldset(readonly: true) {
      div(readonly: false) {
        field "name"
      }
    }
    """ compilesTo 
    <view><div><field readonly="false">name</field></div></view>
  }
  
  
  @Test
  def innerVariableWithCascadeName = { 
    """
    macro div(cascade readonly : bool) with g : generator {
      node div {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      node field(readonly: readonly) s
    }
    fieldset(readonly: true) {
      var readonly = false
      div {
        field "name"
      }
    }
    """ compilesTo 
    <view><div><field readonly="true">name</field></div></view>
  }
  
  /*
   fieldset(readonly: true) {
     field "name"
   }
   
   if readonly has cascade is equivalent to:
   var $readonly = true
   fieldset(readonly: true) { 
     field(readonly: $readonly)
   }
   
   
   */
}