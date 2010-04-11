package uk.co.colinhowe.gleam.feature

import org.junit.Test
import org.junit.Assert._

import uk.co.colinhowe.gleam.compiler.typing.Type
import uk.co.colinhowe.gleam.IdentifierNotFoundError
import uk.co.colinhowe.gleam.IncompatibleControllerError
import uk.co.colinhowe.gleam.compiler.TypeCheckerTest
import uk.co.colinhowe.gleam.compiler.MacroDefinition
import uk.co.colinhowe.gleam.compiler.typing.SimpleType
import uk.co.colinhowe.gleam.compiler.Restriction
import uk.co.colinhowe.gleam.compiler.DummyInterface
import uk.co.colinhowe.gleam.compiler.DummyController

class TestMacroWithController extends TypeCheckerTest {
  
  @Test
  def basicMacroController = {
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    node x with java.lang.Object
    macro p with s : string
        controller uk.co.colinhowe.gleam.compiler.DummyController {
      x c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo 
    <view><x>someString</x></view>
  }
  
  @Test
  def interfaceAsMacroController = {
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    node x with java.lang.Object
    macro p with s : string
        controller uk.co.colinhowe.gleam.compiler.DummyInterface {
      x c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo
    <view><x>someString</x></view>
  }
  
  @Test
  def interfaceAsMacroControllerIgnoresViewController = {
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    macro p with s : string
        controller uk.co.colinhowe.gleam.compiler.DummyInterface {
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
        controller uk.co.colinhowe.gleam.compiler.DummyInterface {
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
        controller uk.co.colinhowe.gleam.compiler.DummyInterface {
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
