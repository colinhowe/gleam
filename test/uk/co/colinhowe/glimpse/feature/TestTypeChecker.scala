package uk.co.colinhowe.glimpse.feature

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.Generator

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.compiler.TypeCheckerTest
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.ArgumentDefinition
import uk.co.colinhowe.glimpse.compiler.Restriction
import uk.co.colinhowe.glimpse.TypeCheckError
import uk.co.colinhowe.glimpse.MacroNotFoundError
import uk.co.colinhowe.glimpse.IdentifierNotFoundError
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.junit.Assert._

class TestTypeChecker extends TypeCheckerTest {
  
  @Test
  def assignStringToIntFails  = {   
    """    
    var x = 1     x = "a"
    """ failsWith
    TypeCheckError(
        line = 3, 
        expectedType = new SimpleType(classOf[Integer]), 
        actualType = new SimpleType(classOf[String]))
  }
  
  @Test
  def assignIntToStringFails  = {
    """    
    var x = "a"    x = 1
    """ failsWith
    TypeCheckError(
        line = 3, 
        expectedType = new SimpleType(classOf[String]), 
        actualType = new SimpleType(classOf[Integer]))
  }
  

  @Test
  def assignStringToStringSuceeds = {   
    """
    var x = "a"
    """ succeeds
  }
  

  @Test
  def assignIntToIntSuceeds = {   
    """
    var x = 1
    """ succeeds
  }
  

  @Test
  def invalidArgumentToMacro = {   
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](),
        Map("v" -> ArgumentDefinition("v", SimpleType(classOf[Integer]), false, false)))

    ;
    """
    macro p(v : int) with s : string {
      var x = 1
    }
    p(v: "1") "1"
    """ failsWith
    MacroNotFoundError(
      line = 5,
      name = "p",
      argumentTypes = Map[String, Type](
        "v" -> SimpleType(classOf[String])
      ),
      valueType = SimpleType(classOf[String]),
      definitionsFound = Set[MacroDefinition](definition)
    )
  }
  

  @Test
  def validCallToMacro = {
    """
    macro p(v : int) with s : string {
       
    }
    p(v: 1) "1"
    """ succeeds
  }
  

  @Test
  def macroWithoutValue = {
    """
    macro p(v : int) {
       
    }
    p(v: 1)
    """ succeeds
  }
  

  @Test
  def invalidValueToMacro = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](),
        Map("v" -> ArgumentDefinition("v", SimpleType(classOf[Integer]), false, false)))

    ;
    """
    macro p(v : int) with s : string {
      var x = 1
    }
    p(v: 1) {
      var x = 1
    }
    """ failsWith
    MacroNotFoundError(
      line = 5,      name = "p",      argumentTypes = Map[String, Type](        "v" -> SimpleType(classOf[Integer])      ),      valueType = SimpleType(classOf[Generator]),      definitionsFound = Set[MacroDefinition](definition)    )
  }  
  @Test  def incrementOnString = {     """    var x = "1"    x++    """ failsWith    TypeCheckError(        line = 3,         expectedType = new SimpleType(classOf[Integer]),         actualType = new SimpleType(classOf[String]))  }
  
  @Test
  def macroWithoutValueCalledWithValue = {   
    """
    macro br {
      var x = 1
    }
    br "hi"
    """ failsWith
    MacroNotFoundError(
      line = 5,
      name = "br",
      argumentTypes = Map[String, Type](),
      valueType = SimpleType(classOf[String]),
      definitionsFound = Set[MacroDefinition](
        new MacroDefinition(
          "br", 
          null, 
          false, 
          Set[Restriction](),
          Map[String, ArgumentDefinition]()
        )
      )
    )
  }
  
  @Test
  def macroWithValueCalledWithoutValue = {   
    """
    node br with string
    br
    """ failsWith
    MacroNotFoundError(
      line = 3,
      name = "br",
      argumentTypes = Map[String, Type](),
      valueType = null,
      definitionsFound = Set[MacroDefinition](
        new MacroDefinition(
          "br", 
          SimpleType(classOf[String]), 
          false, 
          Set[Restriction](),
          Map[String, ArgumentDefinition]()
        )
      )
    )
  }
  
  
  @Test
  def assignToUndeclaredVariable  = {   
    """    
    var x = 1 
    y = 2 
    """ failsWith
    IdentifierNotFoundError(
      line = 3, 
      identifier = "y"
    )
  }
  
  
  // TODO Some refactoring is needed to make this test not needed
  @Test
  def assignToUndeclaredVariableInMacro = {   
    """    
    macro p with s : string {
      y = 2 
    } 
    p "hi" 
    """ failsWith
    IdentifierNotFoundError(
      line = 3, 
      identifier = "y"
    )
  }
}
