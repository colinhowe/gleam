package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import uk.co.colinhowe.glimpse.compiler.node._
import scala.collection.mutable.{ Set => MSet, Buffer }
import scala.collection.JavaConversions._

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._
import uk.co.colinhowe.glimpse.compiler.ArgumentSource._

class TestCallResolver extends AssertionsForJUnit {
  
  private val topLevelInvocation = new AMacroStmt()
  new AView(
    Buffer[PImport](),
    null,
    Buffer[PMacroDefn](),
    Buffer[PStmt](topLevelInvocation)
  )
  
  private val stubCascadeIdentifier = mock(classOf[CascadeIdentifier])
  when(stubCascadeIdentifier.identify(any())).thenReturn(Map[String, Type]())
  
  @Test
  def singleMatchWithNoArguments = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition]())
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val resolvedCall = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), SimpleType(classOf[String]), null)
    val expectedCall = ResolvedCall(definition, Map())
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def noMatchByValueType = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition]())
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), SimpleType(classOf[Generator]), null)
    
    assert(None === definitionFound)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentNames = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition](
        "name" -> ArgumentDefinition("name", SimpleType(classOf[String]), false, false)))

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition](
        "title" -> ArgumentDefinition("title", SimpleType(classOf[String]), false, false)))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val resolvedCall = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type]("title" -> SimpleType(classOf[String])), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    val expectedCall = ResolvedCall(definition2, Map("title" -> Call))
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def twoMacrosWithDifferentArgumentTypes = {
    val definition1 = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition](
        "name" -> ArgumentDefinition("name", SimpleType(classOf[String]), false, false)))

    val definition2 = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition](
        "name" -> ArgumentDefinition("name", SimpleType(classOf[Int]), false, false)))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition1, definition2))

    val resolver = new CallResolver(definitionProvider)

    val resolvedCall = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type]("name" -> SimpleType(classOf[Int])), SimpleType(classOf[String]), null)
    
    val expectedCall = ResolvedCall(definition2, Map("name" -> Call))
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def macroInvokationWithInsufficientArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition](
        "name" -> ArgumentDefinition("name", SimpleType(classOf[String]), false, false)))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    assert(None === definitionFound)
  }
  
  @Test
  def restrictedMacro = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](NameRestriction("restriction")))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)
    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    assert(None === definitionFound)
  }
  
  @Test
  def topLevelMacroPassesRestrictions = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction](NameRestriction("top level")))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)
    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), SimpleType(classOf[String]), stubCascadeIdentifier)
    
    val expectedCall = ResolvedCall(definition, Map())
    assert(Some(expectedCall) === definitionFound)
  }
  
  @Test
  def macroInvocationWithTooManyArguments = {
    val definition = new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction]())
    
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type]("name" -> SimpleType(classOf[String])), SimpleType(classOf[String]), null)
    
    assert(None === definitionFound)
  }
  
  @Test
  def macroInvokationDetectsCascade = {
    val defn = new MacroDefinition("field", SimpleType(classOf[String]), false, Set[Restriction](),
        Map[String, ArgumentDefinition]("readonly" -> ArgumentDefinition("readonly", SimpleType(classOf[Boolean]), true, false)))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](defn))
    
    val resolver = new CallResolver(definitionProvider)
    
    val cascadeIdentifier = mock(classOf[CascadeIdentifier])
    when(cascadeIdentifier.identify(any())).thenReturn(Map[String, Type]("readonly" -> SimpleType(classOf[Boolean])))
    
    val resolvedCall = resolver.getMatchingMacro(
        topLevelInvocation,
        "field", 
        Map[String, Type](), 
        SimpleType(classOf[String]), 
        cascadeIdentifier)
    val expectedCall = ResolvedCall(defn, Map("readonly" -> Cascade))
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def macroInvokationDetectsDefaults = {
    val defn = new MacroDefinition("field", SimpleType(classOf[String]), false, Set[Restriction](),
        Map[String, ArgumentDefinition]("readonly" -> ArgumentDefinition("readonly", SimpleType(classOf[Boolean]), true, true)))

    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](defn))
    
    val resolver = new CallResolver(definitionProvider)
    
    val cascadeIdentifier = mock(classOf[CascadeIdentifier])
    when(cascadeIdentifier.identify(any())).thenReturn(Map[String, Type]())
    
    val resolvedCall = resolver.getMatchingMacro(
        topLevelInvocation,
        "field", 
        Map[String, Type](), 
        SimpleType(classOf[String]), 
        cascadeIdentifier)
    
    val expectedCall = ResolvedCall(defn, Map("readonly" -> Default))
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def ignoresRuntimeTypedImplementations = {
    val abstractDefinition = new MacroDefinition(
        "p", new SimpleType(classOf[String]), false, Set[Restriction](), 
        Map[String, ArgumentDefinition]("o" -> ArgumentDefinition(
          "o", SimpleType(classOf[Object]), false, false, true
        )),
        true
    )
    val concreteDefinition = new MacroDefinition(
        "p", new SimpleType(classOf[String]), false, Set[Restriction](), 
        Map[String, ArgumentDefinition]("o" -> ArgumentDefinition(
          "o", SimpleType(classOf[String]), false, false, true
        )),
        false
    )
    
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](
        concreteDefinition, abstractDefinition))

    val resolver = new CallResolver(definitionProvider)

    val resolvedCall = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type]("o" -> SimpleType(classOf[String])), SimpleType(classOf[String]), null)
    val expectedCall = ResolvedCall(abstractDefinition, Map("o" -> Call))
    assert(Some(expectedCall) === resolvedCall)
  }
  
  @Test
  def macroWithValueCalledWithoutValue = {
    val definition = new MacroDefinition("p", new SimpleType(classOf[String]), false, Set[Restriction](), Map[String, ArgumentDefinition]())
    val definitionProvider = mock(classOf[MacroDefinitionProvider])
    when(definitionProvider.get(any())).thenReturn(Set[MacroDefinition](definition))

    val resolver = new CallResolver(definitionProvider)

    val definitionFound = resolver.getMatchingMacro(topLevelInvocation, "p", Map[String, Type](), null, null)
    
    assert(None === definitionFound)
  }
}