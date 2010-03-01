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
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.junit.Assert._

class TestTypeChecker extends TypeCheckerTest {
  
  @Test
  def assignStringToIntFails  = {   
    """    
    var x = 1
    """ failsWith
    TypeCheckError(
        line = 3, 
        expectedType = new SimpleType(classOf[Integer]), 
        actualType = new SimpleType(classOf[String]))
  }
  
  @Test
  def assignIntToStringFails  = {
    """    
    var x = "a"
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
      node p s 
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
      node p s 
    }
    p(v: 1) "1"
    """ succeeds
  }
  

  @Test
  def macroWithoutValue = {
    """
    macro p(v : int) {
      node p v 
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
      node p s 
    }
    p(v: 1) {
      node div "hi"
    }
    """ failsWith
    MacroNotFoundError(
      line = 5,
  }
  @Test
  
  @Test
  def macroWithoutValueCalledWithValue = {   
    """
    macro br {
      node br
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
    macro br with s : string {
      node br s
    }
    br
    """ failsWith
    MacroNotFoundError(
      line = 5,
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
}