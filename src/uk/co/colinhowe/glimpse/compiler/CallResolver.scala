package uk.co.colinhowe.glimpse.compiler;
import uk.co.colinhowe.glimpse.compiler.typing.GenericType

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.typing.Type
import scala.collection.mutable.{ Map => MMap }

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
  
  def getMacrosWithName(macroName : String) = provider.get(macroName)
  
  private def matches(definition : MacroDefinition, arguments : Map[String, Type], valueType : Type) : Boolean = {
    var definitionToMatch = definition
    
    // Attempt generic bindings so that matches can be performed correctly
    val genericBindings = MMap[String, Type]()
    // TODO Remove this out into a class specifically for generic bindings so that it can be reused
    if (definition.valueType.isInstanceOf[GenericType]) {
      val t = definition.valueType.asInstanceOf[GenericType]
      genericBindings(t.typeId) = valueType
      definitionToMatch = new MacroDefinition(definition.name, valueType, definition.isDynamic)
      for (argument <- arguments) {
        definitionToMatch.addArgument(argument._1, argument._2)
      }
    }
    
    for (argument <- definition.arguments) {
      if (argument._2.isInstanceOf[GenericType]) {
        val t = argument._2.asInstanceOf[GenericType]
        
        if (genericBindings.containsKey(t.typeId)) {
          if (genericBindings(t.typeId) != arguments.get(argument._1)) {
            // Generics can't be consistently bound so this definition does not match
            return false
          }
        } else {
          genericBindings(t.typeId) = arguments(argument._1)
          val oldDefinition = definitionToMatch
          definitionToMatch = new MacroDefinition(definition.name, definition.valueType, definition.isDynamic)
          for (existingArgument <- oldDefinition.arguments) {
            if (existingArgument._1 == argument._1) {
              definitionToMatch.addArgument(argument._1, arguments(argument._1))
            } else{
              definitionToMatch.addArgument(argument._1, argument._2)
            }
          }
        }
      }
    }
    
    println ("Generics resolved to [" + definitionToMatch + "]")
    
    if (!valueType.canBeAssignedTo(definitionToMatch.valueType)) {
      return false
    }
    
    // Check whether all the arguments exist on the definition
    val invocationArgumentsExist = arguments.forall(invokationArg => 
      definitionToMatch.arguments.exists(argumentMatches(invokationArg, _))
    )
    if (!invocationArgumentsExist) {
      return false
    }
    
    // Check whether all the definition's arguments are satisfied
    return definitionToMatch.arguments.forall(arg => arguments.contains(arg._1))
  }
  
  private def argumentMatches(invocationArg : (String, Type), macroArg : (String, Type)) : Boolean = {
    invocationArg._1 == macroArg._1 &&
    invocationArg._2.canBeAssignedTo(macroArg._2)
  }
}