package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.node.AGenericDefn
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.node.ADynamicMacroDefn
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn
import uk.co.colinhowe.glimpse.compiler.node.AArgDefn
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


class MacroDefinitionFinder(
    val lineNumberProvider : LineNumberProvider,
    val typeProvider : TypeProvider,
    val macroProvider : MacroDefinitionProvider)
  extends DepthFirstAdapter {
  
  val errors = ListBuffer[CompilationError]()
  val genericsInScope = scala.collection.mutable.Map[String, Type]()

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
    val contentType = typeProvider.getType(node.getContentType(), Map[String, Type]() ++ genericsInScope)
    val defn = new MacroDefinition(name, contentType, false)

    // Process all the arguments
    for (pargDefn <- node.getArgDefn()) {
      val argDefn = pargDefn.asInstanceOf[AArgDefn]
      println("generics: " + genericsInScope)
      val argType = typeProvider.getType(argDefn.getType(), Map[String, Type]() ++ genericsInScope)
      val argumentName = argDefn.getIdentifier().getText()
      defn.addArgument(argumentName, argType)
    }
    
    println("Found macro [" + defn.getName() + "]")
    macroProvider.add(defn)

    // Clear any generics in scope
    genericsInScope.clear();
    System.out.println("Cleared generics from scope")
  }
  

  override def outADynamicMacroDefn(node : ADynamicMacroDefn) = {
    // Get the name of the macro
    val name = node.getName().getText()
    
    if (macroProvider.get(name).size != 0) {
      errors += new MultipleDefinitionError(lineNumberProvider.getLineNumber(node), name)
    }
    
    // Get the type of the content
    val contentType = typeProvider.getType(node.getContentType())
    val defn = new MacroDefinition(name, contentType, true)

    // Process all the arguments
    val args = scala.collection.mutable.Set[Tuple2[String, String]]();
    for (pargDefn <- node.getArgDefn()) {
      val argDefn = pargDefn.asInstanceOf[AArgDefn]
      val argType = typeProvider.getType(argDefn.getType())
      val argumentName = argDefn.getIdentifier().getText()
      defn.addArgument(argumentName, argType)
    }
    
    println("Found macro [" + defn.getName() + "]")
    macroProvider.add(defn)

    // Clear any generics in scope
    genericsInScope.clear();
    System.out.println("Cleared generics from scope")
  }
  
  override def inAGenericDefn(node : AGenericDefn) {
    val nodeType = typeProvider.getType(node)

    // Put this generic in scope
    System.out.println("Put generic["+node.getIdentifier.getText+"] in scope")
    genericsInScope.put(node.getIdentifier().getText(), nodeType)
  }
}
