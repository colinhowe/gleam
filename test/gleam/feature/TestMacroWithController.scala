package gleam.feature

import org.junit.Test
import org.junit.Assert._

import gleam.compiler.typing.Type
import gleam.IdentifierNotFoundError
import gleam.IncompatibleControllerError
import gleam.compiler.TypeCheckerTest
import gleam.compiler.MacroDefinition
import gleam.compiler.typing.SimpleType
import gleam.compiler.Restriction
import gleam.compiler.DummyInterface
import gleam.compiler.DummyController

class TestMacroWithController extends TypeCheckerTest {
  
  @Test
  def basicMacroController = {
    """
    controller gleam.compiler.DummyController
    node x with java.lang.Object
    macro p with s : string
        controller gleam.compiler.DummyController {
      x c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo 
    <view><x>someString</x></view>
  }
  
  @Test
  def interfaceAsMacroController = {
    """
    controller gleam.compiler.DummyController
    node x with java.lang.Object
    macro p with s : string
        controller gleam.compiler.DummyInterface {
      x c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo
    <view><x>someString</x></view>
  }
  
  @Test
  def interfaceAsMacroControllerIgnoresViewController = {
    """
    controller gleam.compiler.DummyController
    macro p with s : string
        controller gleam.compiler.DummyInterface {
      var x = c.someObject
    }
    p "ignored"
    """ controller(new DummyController) failsWith
    IdentifierNotFoundError(
      line = 5,
      identifier = "someObject"
    )
  }
  
  @Test
  def callMacroWithoutController = {
    """
    node x with string
    macro p with s : string
        controller gleam.compiler.DummyInterface {
      x "ignored"
    }
    p "ignored"
    """ failsWith
    IncompatibleControllerError(
      line = 7,
      name = "p",
      controllerFound = null,
      controllerNeeded = SimpleType(classOf[DummyInterface])
    )
  }
  
  @Test
  def callMacroWithWrongController = {
    """
    controller java.lang.String
    macro p with s : string
        controller gleam.compiler.DummyInterface {
      var x = 1
    }
    p "ignored"
    """  controller("controller") failsWith
    IncompatibleControllerError(
      line = 7,
      name = "p",
      controllerFound = SimpleType(classOf[String]),
      controllerNeeded = SimpleType(classOf[DummyInterface])
    )
  }
}
