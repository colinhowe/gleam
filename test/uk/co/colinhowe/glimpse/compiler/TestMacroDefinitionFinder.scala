package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._

class TestMacroDefinitionFinder extends AssertionsForJUnit {
  
  @Test
  def detectsDynamic = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      new ADynamicMacroModifier,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](),
          new ABoolType,
          new TIdentifier("readonly"),
          new ATrueExpr
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      null,
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(true === macro.isDynamic)
  }
  
  @Test
  def detectsAbstract = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      new AAbstractMacroModifier,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](),
          new ABoolType,
          new TIdentifier("readonly"),
          new ATrueExpr
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      null,
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(true === macro.isAbstract)
  }
  
  @Test
  def detectsRuntimeTyping = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      null,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](new ARuntimetypedModifier),
          new ABoolType,
          new TIdentifier("readonly"),
          null
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      null,
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(
      ArgumentDefinition(
          "readonly", SimpleType(classOf[java.lang.Boolean]), false, false, true) 
      ===
      macro.arguments("readonly")
    )
  }
  
  @Test
  def detectsDefaults = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      null,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](),
          new ABoolType,
          new TIdentifier("readonly"),
          new ATrueExpr
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      null,
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(
      ArgumentDefinition("readonly", SimpleType(classOf[java.lang.Boolean]), false, true) 
      ===
      macro.arguments("readonly")
    )
  }
  
  @Test
  def detectsRestriction = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      null,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](),
          new ABoolType,
          new TIdentifier("readonly"),
          new ATrueExpr
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      new ARestriction(Buffer(new TIdentifier("onlyhere"))),
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(Set(NameRestriction("onlyhere")) === macro.restrictions)
  }
  
  @Test
  def detectsTopLevelRestriction = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
      null,
      new TIdentifier("macro"),
      Buffer[PGenericDefn](),
      Buffer[PArgDefn](
        new AArgDefn(
          Buffer[PModifier](),
          new ABoolType,
          new TIdentifier("readonly"),
          new ATrueExpr
        )
      ),
      new AStringType,
      new TIdentifier("s"),
      new ARestriction(Buffer(new TIdentifier("top level"))),
      new AGenerator(
      )
    )
    
    finder.outAMacroDefn(defn)

    val macros = definitionProvider.get("macro")
    val macro = macros.iterator.next
    assert(Set(NameRestriction("top level")) === macro.restrictions)
  }
}