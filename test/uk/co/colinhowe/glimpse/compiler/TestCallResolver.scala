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
  
  @Test
  def singleMatchWithNoArguments = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false)
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type](), SimpleType(classOf[String]))
    
    assert(Some(definition) === definitionFound)
  }
  
  @Test
  def noMatchByValueType = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false)
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type](), SimpleType(classOf[Generator]))
    
    assert(None === definitionFound)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentNames = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition1.addArgument("name", SimpleType(classOf[String]))

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition2.addArgument("title", SimpleType(classOf[String]))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type]("title" -> SimpleType(classOf[String])), SimpleType(classOf[String]))
    
    assert(Some(definition2) === definitionFound)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentTypes = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition1.addArgument("name", SimpleType(classOf[String]))

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition2.addArgument("name", SimpleType(classOf[Int]))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type]("name" -> SimpleType(classOf[Int])), SimpleType(classOf[String]))
    
    assert(Some(definition2) === definitionFound)
  }
  
  @Test
  def macroInvokationWithInsufficientArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    definition.addArgument("name", SimpleType(classOf[String]))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type](), SimpleType(classOf[String]))
    
    assert(None === definitionFound)
  }
  
  @Test
  def macroInvokationWithTooManyArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false)
    
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro("p", Map[String, Type]("name" -> SimpleType(classOf[String])), SimpleType(classOf[String]))
    
    assert(None === definitionFound)
  }
}