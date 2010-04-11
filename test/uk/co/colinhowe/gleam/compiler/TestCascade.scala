package uk.co.colinhowe.gleam.compiler
import org.junit.Test
import uk.co.colinhowe.gleam.IdentifierNotFoundError

import org.junit.Assert._

class TestCascade extends TypeCheckerTest {
  
  @Test
  def immediateCalls = { 
    """
    node f(readonly : bool) with string
    macro field(readonly: bool) with s : string {
      f(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      field "someField"
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><f readonly="true">someField</f></view>
  }
  
  
  @Test
  def insideNodeGenerators = { 
    """
    node f(readonly : bool) with string
    node div with generator
    macro field(readonly: bool) with s : string {
      f(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      div {
        field "someField"
      }
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><div><f readonly="true">someField</f></div></view>
  }
  
  
  @Test
  def insideMacroGenerators = { 
    """
    node f(readonly : bool) with string
    node d with generator
    macro div with g : generator {
      d {
        include g
      }
    }
    macro field(readonly: bool) with s : string {
      f(readonly: readonly) s
    }
    macro fieldset(cascade readonly: bool) with s : string {
      div {
        field "someField"
      }
    }
    fieldset(readonly: true) "ignored"
    """ compilesTo 
    <view><d><f readonly="true">someField</f></d></view>
  }
  
  
  @Test
  def insideIncludes = { 
    """
    node f(readonly : bool) with string
    node d with generator
    macro div with g : generator {
      d {
        include g
      }
    }
    macro field(readonly: bool) with s : string {
      f(readonly: readonly) s
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
    <view><d><f readonly="true">hi</f></d></view>
  }
  
  
  @Test
  def noCascadeIntoOtherMacros = { 
    """
    node x(readonly : bool) with string
    node p with string
    macro div with g : generator {
      x(readonly: readonly) "x"
    }
    macro fieldset(cascade readonly: bool) with s : string {
      div {
        p "fail"
      }
    }
    fieldset(readonly: true) "fail"
    """ failsWith
    IdentifierNotFoundError(
      line = 5,
      identifier = "readonly"
    )
  }
  
  
  @Test
  def innerVariableSameNameButNoCascade = { 
    """
    node f(readonly : bool) with string
    node d with generator
    macro div(readonly : bool) with g : generator {
      d {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      f(readonly: readonly) s
    }
    fieldset(readonly: true) {
      div(readonly: false) {
        field "name"
      }
    }
    """ compilesTo 
    <view><d><f readonly="true">name</f></d></view>
  }
  
  
  @Test
  def innerVariableSameNameAndCascade = { 
    """
    node f(readonly : bool) with string
    node d with generator
    macro div(cascade readonly : bool) with g : generator {
      d {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      f(readonly: readonly) s
    }
    fieldset(readonly: true) {
      div(readonly: false) {
        field "name"
      }
    }
    """ compilesTo 
    <view><d><f readonly="false">name</f></d></view>
  }
  
  
  @Test
  def innerVariableWithCascadeName = { 
    """
    node f(readonly : bool) with string
    node d with generator
    macro div(cascade readonly : bool) with g : generator {
      d {
        include g
      }
    }
    macro fieldset(cascade readonly : bool) with g : generator {
      include g
    }
    macro field(readonly : bool) with s : string {
      f(readonly: readonly) s
    }
    fieldset(readonly: true) {
      var readonly = false
      div {
        field "name"
      }
    }
    """ compilesTo 
    <view><d><f readonly="true">name</f></d></view>
  }
}