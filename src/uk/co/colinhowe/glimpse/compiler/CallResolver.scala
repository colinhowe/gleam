package uk.co.colinhowe.glimpse.compiler;

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.typing.Type

import scala.collection.JavaConversions._

class CallResolver(provider : MacroDefinitionProvider) extends DepthFirstAdapter {
  def getMatchingMacro(macroName : String, arguments : Map[String, Type], valueType : Type) : Option[MacroDefinition] = {
    val definitions = provider.get(macroName)
    val matchingDefinitions = definitions.filter(matches(_, arguments, valueType))
    val iterator = matchingDefinitions.iterator
    if (iterator.hasNext) {
      Some(iterator.next)
    } else {
      None
    }
  }
  
  private def matches(definition : MacroDefinition, arguments : Map[String, Type], valueType : Type) : Boolean = {
    if (!valueType.canBeAssignedTo(definition.valueType)) {
      return false
    }
    
    // Check whether all the arguments exist on the definition
    val invocationArgumentsExist = arguments.forall(invokationArg => 
      definition.arguments.exists(argumentMatches(invokationArg, _))
    )
    if (!invocationArgumentsExist) {
      return false
    }
    
    // Check whether all the definition's arguments are satisfied
    return definition.arguments.forall(arg => arguments.contains(arg._1))
  }
  
  private def argumentMatches(invocationArg : (String, Type), macroArg : (String, Type)) : Boolean = {
    invocationArg._1 == macroArg._1 &&
    invocationArg._2.canBeAssignedTo(macroArg._2)
  }
}