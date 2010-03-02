package uk.co.colinhowe.glimpse.feature

import org.junit.Test
import org.junit.Assert._

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.IdentifierNotFoundError
import uk.co.colinhowe.glimpse.IncompatibleControllerError
import uk.co.colinhowe.glimpse.compiler.TypeCheckerTest
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.Restriction
import uk.co.colinhowe.glimpse.compiler.DummyInterface
import uk.co.colinhowe.glimpse.compiler.DummyController

class TestMacroWithController extends TypeCheckerTest {
  
  @Test
  def basicMacroController = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    macro p with s : string
        controller uk.co.colinhowe.glimpse.compiler.DummyController {
      node p c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo 
    <view><p>someString</p></view>
  }
  
  @Test
  def interfaceAsMacroController = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    macro p with s : string
        controller uk.co.colinhowe.glimpse.compiler.DummyInterface {
      node p c.someString
    }
    p "ignored"
    """ controller(new DummyController) compilesTo
    <view><p>someString</p></view>
  }
  
  @Test
  def interfaceAsMacroControllerIgnoresViewController = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    macro p with s : string
        controller uk.co.colinhowe.glimpse.compiler.DummyInterface {
      node p c.someObject
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
    macro p with s : string
        controller uk.co.colinhowe.glimpse.compiler.DummyInterface {
      node p c.someObject
    }
    p "ignored"
    """ failsWith
    IncompatibleControllerError(
      line = 6,
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
        controller uk.co.colinhowe.glimpse.compiler.DummyInterface {
      node p c.someObject
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
