package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import scala.math._

case class MacroDefinition(
    val name : String, 
    val valueType : Type, 
    val isDynamic : Boolean,
    val restrictions : Iterable[Restriction],
    val arguments : Map[String, ArgumentDefinition] = Map(),
    val isAbstract : Boolean = false,
    val controller : Type = null,
    val isNodeDefn : Boolean = false) extends Type {
  
  def hasRuntimeTyping = {
    arguments.exists(arg => arg._2.isRuntimeTyped)
  }
  
  override def equals(o : Any) : Boolean = {
    o match {
      case defn : MacroDefinition => 
        defn.name == name &&
        defn.valueType == valueType &&
        defn.isDynamic == isDynamic &&
        defn.arguments == arguments
      case _ => false
    }
  }
  
  override def toString : String = {
    "MacroDefinition("+name+","+valueType+","+isDynamic+","+arguments+")"
  }
  
  /**
   * The class name is built up from the name plus a hash of:
   *  - the argument names and types
   *  - the value type
   * @return
   */
  def className : String = {
    if (hasRuntimeTyping && !isAbstract) {
      val runtimeArgument = arguments.find(arg => arg._2.isRuntimeTyped).get._2
      val toHash = runtimeArgument.argType.asInstanceOf[SimpleType].clazz.getCanonicalName
      return name + "$rtt" + Integer.toString(abs(toHash.hashCode()), 16)
    } else if (!isDynamic && !isAbstract) {
      var toHash = name + valueType
      for ((key, value) <- arguments) {
        toHash += key + value
      }
      return name + "$" + Integer.toString(abs(toHash.hashCode()), 16)
    } else {
      return name
    }
  }
  
  def areCompatible(other : MacroDefinition) = {
    other.arguments.equals(arguments) && other.valueType.equals(valueType)
  }

  def canBeAssignedTo(assignee : Type) : Boolean = {
    throw new UnsupportedOperationException("Not implemented")
  }
}