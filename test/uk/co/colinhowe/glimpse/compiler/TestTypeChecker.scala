package uk.co.colinhowe.glimpse.compiler
import uk.co.colinhowe.glimpse.Generator


import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.TypeCheckError
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType

import org.junit.Assert._

class TestTypeChecker extends TypeCheckerTest {
  
  @Test
  def assignStringToIntFails  = {   
    """    
    int x = "a"
    """ failsWith
    TypeCheckError(
        line = 2, 
        expectedType = new SimpleType(classOf[Integer]), 
        actualType = new SimpleType(classOf[String]))
  }
  
  @Test
  def assignIntToStringFails  = {
    """    
    string x = 1
    """ failsWith
    TypeCheckError(
        line = 2, 
        expectedType = new SimpleType(classOf[String]), 
        actualType = new SimpleType(classOf[Integer]))
  }
  

  @Test
  def assignStringToStringSuceeds = {   
    """
    string x = "a"
    """ succeeds
  }
  

  @Test
  def assignIntToIntSuceeds = {   
    """
    int x = 1
    """ succeeds
  }
  

  @Test
  def invalidArgumentToMacro = {   
    """
    macro p(int v) with string s { node p s }
    p(v: "1") "1"
    """ failsWith
    TypeCheckError(
        line = 3, 
        expectedType = new SimpleType(classOf[Integer]), 
        actualType = new SimpleType(classOf[String]))
  }
  

  @Test
  def validCallToMacro = {
    """
    macro p(int v) with string s { node p s }
    p(v: 1) "1"
    """ succeeds
  }
  

  @Test
  def invalidValueToMacro = {   
    """
    macro p(int v) with string s { node p s }
    p(v: 1) {
      node div "hi"
    }
    """ failsWith
    TypeCheckError(
        line = 3, 
        expectedType = new SimpleType(classOf[String]), 
        actualType = new SimpleType(classOf[Generator]))  }  
  @Test  def incrementOnString = {     """    string x = "1"    x++    node h1 x    """ failsWith    TypeCheckError(        line = 3,         expectedType = new SimpleType(classOf[Integer]),         actualType = new SimpleType(classOf[String]))  }
}
