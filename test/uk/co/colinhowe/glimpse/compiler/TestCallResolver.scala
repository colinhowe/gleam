package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.Set

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._

class TestCallResolver extends AssertionsForJUnit {
  
  private val stubCascadeIdentifier = mock(classOf[CascadeIdentifier])
  when(stubCascadeIdentifier.identify(any())).thenReturn(Map[String, Type]())
  
  @Test
  def singleMatchWithNoArguments = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false)
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type](), SimpleType(classOf[String]), null)
    
    assert(Some(definition) === definitionFound)
  }
  
  @Test
  def noMatchByValueType = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false)
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type](), SimpleType(classOf[Generator]), null)
    
    assert(None === definitionFound)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentNames = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition1.addArgument("name", SimpleType(classOf[String]), false)

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition2.addArgument("title", SimpleType(classOf[String]), false)

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type]("title" -> SimpleType(classOf[String])), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    assert(Some(definition2) === definitionFound)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentTypes = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition1.addArgument("name", SimpleType(classOf[String]), false)

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition2.addArgument("name", SimpleType(classOf[Int]), false)

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type]("name" -> SimpleType(classOf[Int])), SimpleType(classOf[String]), null)
    
    assert(Some(definition2) === definitionFound)
  }
  
  @Test
  def macroInvokationWithInsufficientArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition.addArgument("name", SimpleType(classOf[String]), false)

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type](), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    assert(None === definitionFound)
  }
  
  @Test
  def macroInvokationWithTooManyArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(null, "p", Map[String, Type]("name" -> SimpleType(classOf[String])), SimpleType(classOf[String]), null)
    
    assert(None === definitionFound)
  }
  
  @Test
  def macroInvokationDetectsCascade = {
    val defn = new MacroDefinition("field", SimpleType(classOf[String]), false)
    defn.addArgument("readonly", SimpleType(classOf[Boolean]), true)

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](defn))
    
    val resolver = new CallResolver(definitionProvider)
    
    val cascadeIdentifier = mock(classOf[CascadeIdentifier])
    when(cascadeIdentifier.identify(any())).thenReturn(Map[String, Type]("readonly" -> SimpleType(classOf[Boolean])))
    
    val definitionFound = resolver.getMatchingMacro(
        null,
        "field", 
        Map[String, Type](), 
        SimpleType(classOf[String]), 
        cascadeIdentifier)    
    assert(Some(defn) === definitionFound)
  }
}