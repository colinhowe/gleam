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
  def detectsDefaults = {
    val definitionProvider = new MacroDefinitionProvider
    val finder = new MacroDefinitionFinder(
        null, 
        new TypeProvider, 
        definitionProvider, 
        null)
    
    val defn = new AMacroDefn(
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
}