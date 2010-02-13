package uk.co.colinhowe.glimpse.compiler;

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node.AArgDefn
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn
import uk.co.colinhowe.glimpse.compiler.node.PArgDefn
import uk.co.colinhowe.glimpse.compiler.typing.Type

import scala.collection.JavaConversions._

class CallResolver(
    val lineNumberProvider : LineNumberProvider, 
    val typeResolver : TypeResolver,
    typeNameResolver : TypeNameResolver
  ) extends DepthFirstAdapter {
  
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  val macros = scala.collection.mutable.Map[String, scala.collection.mutable.Set[MacroDefinition]]()
  
  override def outAMacroDefn(node : AMacroDefn) {
    // Get the name of the macro
    val name = node.getName().getText()
    
    if (!macros.contains(name)) {
      macros.put(name, scala.collection.mutable.Set[MacroDefinition]())
    } else {
      errors += new MultipleDefinitionError(
          lineNumberProvider.getLineNumber(node), name)
    }
    
    // Get the type of the content
    val contentType = typeResolver.getType(node.getContentType(), typeNameResolver, null)
    val defn = new MacroDefinition(name, contentType, false)

    // Process all the arguments
    for (pargDefn <- node.getArgDefn()) {
      val argDefn = pargDefn.asInstanceOf[AArgDefn]
      val t = typeResolver.getType(argDefn.getType(), typeNameResolver, null)
      val argumentName = argDefn.getIdentifier().getText()
      defn.addArgument(argumentName, t)
    }
    
    macros(name).add(defn)
  }

  def getMacrosWithName(macroName : String) = {
    if (macros.containsKey(macroName)) {
      macros(macroName)
    } else {
      scala.collection.mutable.Set[MacroDefinition]()
    }
  }
}