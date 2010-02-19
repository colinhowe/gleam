package uk.co.colinhowe.glimpse.compiler;
import uk.co.colinhowe.glimpse.compiler.typing.CompoundType

import uk.co.colinhowe.glimpse.compiler.typing.SimpleType

import uk.co.colinhowe.glimpse.compiler.typing.GenericType

import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.MultipleDefinitionError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.typing.Type
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.Buffer

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
  
  def resolveGenerics(source : Type, target : Type, bindings : Map[String, Type]) : (Type, Map[String, Type]) = {
    target match {
      case t : SimpleType => (t, bindings)
      case target : CompoundType => 
        // If the source type doesn't match then we can bail now
        if (!source.isInstanceOf[CompoundType]) {
          (target, bindings)
        } else {
          var currentBindings = bindings
          var newInnerTypes = Buffer[Type]()
          val compoundSource = source.asInstanceOf[CompoundType]

          for (i <- 0 until target.innerTypes.size) {
            val (newInnerType, newBindings) = resolveGenerics(
                compoundSource.innerTypes(i), target.innerTypes(i), currentBindings)
            currentBindings = newBindings
            newInnerTypes += newInnerType
          }
          
          (CompoundType(target.clazz, newInnerTypes.toList), currentBindings)
        }
      case t : GenericType =>
        val newBindings = bindings + (t.typeId -> source)
        (source, newBindings)
      case _ =>
        throw new RuntimeException("Fail")
    }
  }
  
  private def matches(definition : MacroDefinition, arguments : Map[String, Type], valueType : Type) : Boolean = {
    var definitionToMatch = definition
    
    // Attempt generic bindings so that matches can be performed correctly
    // TODO Remove this out into a class specifically for generic bindings so that it can be reused
    val (newValueType, genericBindings) = 
      resolveGenerics(valueType, definition.valueType, Map[String, Type]())
    definitionToMatch = new MacroDefinition(definition.name, newValueType, definition.isDynamic)
    
    var currentBindings = genericBindings
    for (argument <- definition.arguments) {
      if (!arguments.contains(argument._1)) {
        return false
      }
      
      // TODO Fix this kludge
      var (newArgType2, currentBindings2) = resolveGenerics(arguments(argument._1), argument._2, currentBindings)
      currentBindings = currentBindings2
      
      definitionToMatch.addArgument(argument._1, newArgType2)
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