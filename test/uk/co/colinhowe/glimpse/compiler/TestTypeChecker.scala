package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.Generator

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.TypeCheckError
import uk.co.colinhowe.glimpse.MacroNotFoundError
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.junit.Assert._

class TestTypeChecker extends TypeCheckerTest {
  
  @Test
  def assignStringToIntFails  = {   
    """    
    var x = 1    x = "a"
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
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition.addArgument("v", SimpleType(classOf[Integer]))

    ;
    """
    macro p(v : int) with s : string { node p s }
    p(v: "1") "1"
    """ failsWith
    MacroNotFoundError(
      line = 3,
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
    macro p(v : int) with s : string { node p s }
    p(v: 1) "1"
    """ succeeds
  }
  

  @Test
  def invalidValueToMacro = {   
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition.addArgument("v", SimpleType(classOf[Integer]))

    ;
    """
    macro p(v : int) with s : string { node p s }
    p(v: 1) {
      node div "hi"
    }
    """ failsWith
    MacroNotFoundError(
      line = 3,      name = "p",      argumentTypes = Map[String, Type](        "v" -> SimpleType(classOf[Integer])      ),      valueType = SimpleType(classOf[Generator]),      definitionsFound = Set[MacroDefinition](definition)    )
  }  
  @Test  def incrementOnString = {     """    var x = "1"    x++    node h1 x    """ failsWith    TypeCheckError(        line = 3,         expectedType = new SimpleType(classOf[Integer]),         actualType = new SimpleType(classOf[String]))  }
}
