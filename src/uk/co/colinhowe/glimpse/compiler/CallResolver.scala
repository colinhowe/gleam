package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
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
import uk.co.colinhowe.glimpse.compiler.ArgumentSource._

class CallResolver(
    provider : MacroDefinitionProvider
    
) extends DepthFirstAdapter {
  def getMatchingMacro(node : Node, macroName : String, arguments : Map[String, Type], valueType : Type, cascadeIdentifier : CascadeIdentifier) : Option[ResolvedCall] = {
    val definitions = provider.get(macroName)
    val calls = definitions.map(matches(node, _, arguments, valueType, cascadeIdentifier))
    val matchingDefinitions = calls.filter(_ != None)
    val iterator = matchingDefinitions.iterator
    if (iterator.hasNext) {
      iterator.next
    } else {
      None
    }
  }
  
  def getParent(node : Node) : String = {
    node match {
      case stmt : AMacroStmt => stmt.toString
      case defn : AMacroDefn => defn.getName.getText
      case view : AView => "top level"
      case node : Node => getParent(node.parent)
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
  
  private def matches(node : Node, definition : MacroDefinition, arguments : Map[String, Type], valueType : Type, cascadeIdentifier : CascadeIdentifier) : Option[ResolvedCall] = {
    var definitionToMatch = definition
    
    val argumentSources = MMap[String, ArgumentSource]()
    
    // Check restrictions on this definition
    val parent = getParent(node.parent)
    if (definition.restrictions .size > 0) {
      var restricted = true
      for (restriction <- definition.restrictions) {
        restricted &= (restriction match {
          case NameRestriction(name) => name != parent 
        })
      }
      if (restricted) {
        return None
      }
    }
    
    // Attempt generic bindings so that matches can be performed correctly
    // TODO Remove this out into a class specifically for generic bindings so that it can be reused
    val (newValueType, genericBindings) = 
      resolveGenerics(valueType, definition.valueType, Map[String, Type]())
    
    var currentBindings = genericBindings
    val argumentsToMatch = MMap[String, ArgumentDefinition]()
    for ((name, defn) <- definition.arguments) {
      if (!arguments.contains(name)) {
        // Check if a cascade can be applied
        val cascadedArg = cascadeIdentifier.identify(node).get(name)
        cascadedArg match {
          case Some(cascadedArg) => 
            if (!cascadedArg.canBeAssignedTo(defn.argType)) {
              return None
            } else {
              argumentSources(name) = Cascade
              argumentsToMatch(name) = ArgumentDefinition(name, defn.argType, defn.cascade, defn.hasDefault)
            }
          case None => 
            if (!defn.hasDefault) {
              return None
            } else {
              argumentSources(name) = Default
              argumentsToMatch(name) = ArgumentDefinition(name, defn.argType, defn.cascade, defn.hasDefault)
            }
        }
      } else {
        // TODO Fix this kludge
        var (newArgType2, currentBindings2) = resolveGenerics(arguments(name), defn.argType, currentBindings)
        currentBindings = currentBindings2
        argumentSources(name) = Call
        argumentsToMatch(name) = ArgumentDefinition(name, newArgType2, defn.cascade, defn.hasDefault)
      }
    }
    
    definitionToMatch = new MacroDefinition(definition.name, newValueType, definition.isDynamic, Set[Restriction](), argumentsToMatch.toMap)

    if (!valueType.canBeAssignedTo(definitionToMatch.valueType)) {
      return None
    }
    
    // Check whether all the arguments exist on the definition
    val invocationArgumentsExist = arguments.forall(invokationArg => 
      definitionToMatch.arguments.exists(argumentMatch(invokationArg, _))
    )
    if (!invocationArgumentsExist) {
      return None
    }
    
    return Some(ResolvedCall(definition, argumentSources.toMap))
  }
  
  private def argumentMatches(invocationArg : (String, Type), macroArg : ArgumentDefinition) : Boolean = {
    invocationArg._1 == macroArg.name &&
    invocationArg._2.canBeAssignedTo(macroArg.argType)
  }
  
  private def argumentMatch(invocationArg : (String, Type), macroArg : (String, ArgumentDefinition)) : Boolean = {
    argumentMatches(invocationArg, macroArg._2)
  }
}