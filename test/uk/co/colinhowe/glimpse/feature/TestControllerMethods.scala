package uk.co.colinhowe.glimpse.feature

import org.junit.Test
import org.junit.Assert._

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.IdentifierNotFoundError
import uk.co.colinhowe.glimpse.MethodNotFoundError
import uk.co.colinhowe.glimpse.IncompatibleControllerError
import uk.co.colinhowe.glimpse.compiler.TypeCheckerTest
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.Restriction
import uk.co.colinhowe.glimpse.compiler.DummyInterface
import uk.co.colinhowe.glimpse.compiler.DummyController

class TestControllerMethods extends TypeCheckerTest {
  
  @Test
  def withArgsInvocation = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    p c.makeMessage("hi")
    """ controller(new DummyController) compilesTo 
    <view><p>hi.message</p></view>
  }
  
  @Test
  def onInterface = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyInterface
    node p with java.lang.Object
    p c.getSomeString()
    """ controller(new DummyController) compilesTo 
    <view><p>someString</p></view>
  }
  
  @Test
  def withLocalVariableInvocation = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    var x = "hi"
    p c.makeMessage(x)
    """ controller(new DummyController) compilesTo 
    <view><p>hi.message</p></view>
  }
  
  @Test
  def withoutArgsInvocation = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    p c.getName()
    """ controller(new DummyController) compilesTo 
    <view><p>Name of the controller</p></view>
  }
  
  @Test
  def returnValueTyping = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    var message = c.makeMessage("hi")
    p message
    """ controller(new DummyController) compilesTo 
    <view><p>hi.message</p></view>
  }

  @Test
  def nonexistentMethod = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    var message = c.makeCompileFailure()
    """ controller(new DummyController) failsWith
    MethodNotFoundError(
      line = 4,
      identifier = "makeCompileFailure",
      arguments = List()
    )
  }

  @Test
  def incorrectArgumentsToMethod = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    var message = c.makeMessage(4)
    """ controller(new DummyController) failsWith
    MethodNotFoundError(
      line = 4,
      identifier = "makeMessage",
      arguments = List(SimpleType(classOf[Integer]))
    )
  }

  @Test
  def incorrectVariableToMethod = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    node p with string
    var x = 4
    var message = c.makeMessage(x)
    """ controller(new DummyController) failsWith
    MethodNotFoundError(
      line = 5,
      identifier = "makeMessage",
      arguments = List(SimpleType(classOf[Integer]))
    )
  }
}
