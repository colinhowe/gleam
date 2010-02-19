package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.Type

case class MacroDefinition(val name : String, val valueType : Type, val isDynamic : Boolean) extends Type {
  // TODO Make arguments part of the constructor
  val arguments = scala.collection.mutable.Map[String, Type]()
  
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
    if (!isDynamic) {
      var toHash = name + valueType
      for ((key, value) <- arguments) {
        toHash += key + value
      }
      println("Hashing [" + toHash + "]")
      return name + "$" + Integer.toString(Math.abs(toHash.hashCode()), 16)
    } else {
      return name
    }
  }
  
  def addArgument(argumentName : String, t : Type) {
    arguments(argumentName) = t
  }
  
  def areCompatible(other : MacroDefinition) = {
    other.arguments.equals(arguments) && other.valueType.equals(valueType)
  }

  def canBeAssignedTo(assignee : Type) : Boolean = {
    throw new UnsupportedOperationException("Not implemented")
  }
}