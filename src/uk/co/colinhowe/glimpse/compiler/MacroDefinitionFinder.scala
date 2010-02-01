package uk.co.colinhowe.glimpse.compiler
import uk.co.colinhowe.glimpse.compiler.node.ADynamicMacroDefn

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn
import uk.co.colinhowe.glimpse.compiler.node.AArgDefn
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


class MacroDefinitionFinder(
    val typeProvider : TypeProvider, 
    val lineNumberProvider : LineNumberProvider, 
    val macroProvider : MacroDefinitionProvider)
  extends DepthFirstAdapter {
  
  val errors = ListBuffer[CompilationError]()
  
  def errorsAsJavaList() : java.util.List[CompilationError] = {
    errors
  }
  
  override def outAMacroDefn(node : AMacroDefn) = {
    // Get the name of the macro
    val name = node.getName().getText()
    
    if (macroProvider.get(name).size != 0) {
      errors += new MultipleDefinitionError(lineNumberProvider.getLineNumber(node), name)
    }
    
    // Get the type of the content
    val contentType = typeProvider.getType(node.getContentType())
    val defn = new MacroDefinition(name, contentType)

    // Process all the arguments
    for (pargDefn <- node.getArgDefn()) {
      val argDefn = pargDefn.asInstanceOf[AArgDefn]
      val argType = typeProvider.getType(argDefn.getType())
      val argumentName = argDefn.getIdentifier().getText()
      defn.addArgument(argumentName, argType)
    }
    
    println("Found macro [" + defn.getName() + "]")
    macroProvider.add(defn)
  }
  

  override def outADynamicMacroDefn(node : ADynamicMacroDefn) = {
    // Get the name of the macro
    val name = node.getName().getText()
    
    if (macroProvider.get(name).size != 0) {
      errors += new MultipleDefinitionError(lineNumberProvider.getLineNumber(node), name)
    }
    
    // Get the type of the content
    val contentType = typeProvider.getType(node.getContentType())
    val defn = new MacroDefinition(name, contentType)

    // Process all the arguments
    for (pargDefn <- node.getArgDefn()) {
      val argDefn = pargDefn.asInstanceOf[AArgDefn]
      val argType = typeProvider.getType(argDefn.getType())
      val argumentName = argDefn.getIdentifier().getText()
      defn.addArgument(argumentName, argType)
    }
    
    println("Found macro [" + defn.getName() + "]")
    macroProvider.add(defn)
  }
}