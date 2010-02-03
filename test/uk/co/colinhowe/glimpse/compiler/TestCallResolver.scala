package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node.AArgDefn
import org.mockito.Matchers._
import uk.co.colinhowe.glimpse.compiler.node.Node
import org.scalatest.junit.AssertionsForJUnit

import java.util.LinkedList
import java.util.List
import java.util.Set

import org.junit.Test

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import uk.co.colinhowe.glimpse.compiler.node.AIntType
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn
import uk.co.colinhowe.glimpse.compiler.node.AStringType
import uk.co.colinhowe.glimpse.compiler.node.PArgDefn
import uk.co.colinhowe.glimpse.compiler.node.TIdentifier
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType

import org.mockito.Mockito._

class TestCallResolver extends AssertionsForJUnit {
  
  @Test
  def testSingleResolution = {
    val lineNumberProvider = mock(classOf[LineNumberProvider])
    val typeProvider = mock(classOf[TypeResolver])
    when(typeProvider.getType(any(), any())).thenReturn(new SimpleType(classOf[Integer]))

    val resolver = new CallResolver(lineNumberProvider, typeProvider)
    val macroDefn = new AMacroDefn()
    val pargId = new TIdentifier("arg")
    val parg = new AArgDefn(null, pargId)
    val pargs = new LinkedList[PArgDefn]
    pargs.add(parg)
    macroDefn.setArgDefn(pargs)
    macroDefn.setContentName(new TIdentifier("s"))
    macroDefn.setContentType(new AStringType())
    macroDefn.setName(new TIdentifier("p"))
    
    resolver.outAMacroDefn(macroDefn)
    
    val macros = resolver.getMacrosWithName("p")
    assert(1 === macros.size())
    
    val macro = macros.iterator().next()
    assert("p" === macro.getName())
    
    val arguments = macro.getArguments()
    assert(1 === arguments.size())
    assert(new SimpleType(classOf[Integer]) === arguments.get("arg"))
  }
  
  @Test
  def testMacrosWithSameName : Unit = {
    val lineNumberProvider = mock(classOf[LineNumberProvider])
    when(lineNumberProvider.getLineNumber(any())).thenReturn(2)
    val typeProvider = mock(classOf[TypeResolver])
    when(typeProvider.getType(any(), any())).thenReturn(new SimpleType(classOf[Integer]))
    val resolver = new CallResolver(lineNumberProvider, typeProvider)

    val macroDefn = new AMacroDefn()
    macroDefn.setArgDefn(new LinkedList[PArgDefn]())
    macroDefn.setContentName(new TIdentifier("s"))
    macroDefn.setContentType(new AStringType())
    macroDefn.setName(new TIdentifier("p"))
    resolver.outAMacroDefn(macroDefn)

    val macroDefn2 = new AMacroDefn()
    macroDefn2.setArgDefn(new LinkedList[PArgDefn]())
    macroDefn2.setContentName(new TIdentifier("s"))
    macroDefn2.setContentType(new AIntType())
    macroDefn2.setName(new TIdentifier("p"))
    resolver.outAMacroDefn(macroDefn2)
    
    // The second definition of p should have resulted in an error
    val errors = resolver.getErrors()
    assert(1 === errors.size())
    val error = errors.get(0).asInstanceOf[MultipleDefinitionError]
    assert(2 === error.lineNumber)
    assert("p" === error.macroName)
    verify(lineNumberProvider).getLineNumber(any())
  }
}