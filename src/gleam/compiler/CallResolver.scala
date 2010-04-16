package gleam.compiler

import gleam.compiler.node._
import gleam.compiler.typing.CompoundType
import gleam.compiler.typing.SimpleType
import gleam.compiler.typing.GenericType
import gleam.CompilationError
import gleam.MultipleDefinitionError
import gleam.compiler.analysis.DepthFirstAdapter
import gleam.compiler.typing.Type
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.Buffer

import scala.collection.JavaConversions._
import gleam.compiler.ArgumentSource._

class CallResolver(provider : MacroDefinitionProvider) extends DepthFirstAdapter {

  def getMatchingMacro(node : Node, macroName : String, arguments : Map[String, Type], valueType : Type, cascadeIdentifier : CascadeIdentifier) : Option[ResolvedCall] = {
    val definitions = provider.get(macroName)
    val calls = definitions.map(matches(node, _, arguments, valueType, cascadeIdentifier))
    val matchingDefinitions = calls.filter(_ != None)
    matchingDefinitions.headOption.getOrElse(None)
  }
  
  private def getParent(node : Node) : String = {
    node match {
      case stmt : AMacroStmt => stmt.getIdentifier.getText
      case defn : AMacroDefn => defn.getName.getText
      case defn : ANodeDefn => defn.getName.getText
      case view : AView => "top level"
      case node : Node => getParent(node.parent)
    }
  }
  
  def getMacrosWithName(macroName : String) = provider.get(macroName)
  
  def resolveGenerics(source : Type, target : Type, bindings : Map[String, Type]) : (Type, Map[String, Type]) = {
    target match {
      case t : SimpleType => (t, bindings)
      case null => (target, bindings)
      case target : CompoundType => 
        // If the source type doesn't match then we can bail now
         source match {
           case source : CompoundType =>
             var currentBindings = bindings
             var newInnerTypes = Buffer[Type]()
  
             source.innerTypes.zip(target.innerTypes).foreach { case(sourceType, targetType) =>
               val (newInnerType, newBindings) = resolveGenerics(sourceType, targetType, currentBindings)
               currentBindings = newBindings
               newInnerTypes += newInnerType
             }
             (CompoundType(target.clazz, newInnerTypes.toList), currentBindings)

           case _ => (target, bindings)
        }
      case t : GenericType => (source, bindings + (t.typeId -> source))
      case _ => throw new RuntimeException("Fail " + target)
    }
  }
  
  private def matches(node : Node, definition : MacroDefinition, arguments : Map[String, Type], valueType : Type, cascadeIdentifier : CascadeIdentifier) : Option[ResolvedCall] = {
    
    // Ignore any definitions that have runtime typing but are not abstract
    if (definition.hasRuntimeTyping && !definition.isAbstract) {
      return None
    }
    
    // Check restrictions on this definition
    val parent = getParent(node.parent)
    var restricted = definition.restrictions.forall(_ match {
      case NameRestriction(name) => name != parent 
    })
    if (definition.restrictions.size > 0 && restricted) {
      return None
    }
    
    // Attempt generic bindings so that matches can be performed correctly
    // TODO Remove this out into a class specifically for generic bindings so that it can be reused
    val (newValueType, genericBindings) = 
      resolveGenerics(valueType, definition.valueType, Map[String, Type]())
    
    var currentBindings = genericBindings
    val argumentsToMatch = MMap[String, ArgumentDefinition]()
    val argumentSources = MMap[String, ArgumentSource]()
    for ((name, defn) <- definition.arguments) {
      val (source, argType) = if (!arguments.contains(name)) {
        // Check if a cascade can be applied
        val cascadedArg = cascadeIdentifier.identify(node).get(name)
        val source = cascadedArg match {
          case Some(cascadedArg) => 
            if (!cascadedArg.canBeAssignedTo(defn.argType)) {
              return None
            } else {
              Cascade
            }
          case None => 
            if (!defn.hasDefault) {
              return None
            } else {
              Default
            }
        }
        (source, defn.argType)
      } else {
        // TODO Fix this kludge
        var (newArgType, currentBindings2) = resolveGenerics(arguments(name), defn.argType, currentBindings)
        currentBindings = currentBindings2
        (Call, newArgType)
      }
      argumentSources(name) = source
      argumentsToMatch(name) = ArgumentDefinition(name, argType, defn.cascade, defn.hasDefault)
    }
    
    val definitionToMatch = new MacroDefinition(definition.name, newValueType, definition.isDynamic, Set[Restriction](), argumentsToMatch.toMap)

    if (valueType != null && !valueType.canBeAssignedTo(definitionToMatch.valueType)) {
      return None
    } else if (valueType == null && definitionToMatch.valueType != null) {
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
  
  private def argumentMatch(invocationArg : (String, Type), macroArg : (String, ArgumentDefinition)) : Boolean = {
    invocationArg._1 == macroArg._2.name &&
      invocationArg._2.canBeAssignedTo(macroArg._2.argType)
  }
}