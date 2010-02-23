package uk.co.colinhowe.glimpse.compiler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SynchronizedBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.{ Set => MSet}

class MacroDefinitionProvider {
  val macros = Map.empty[String, MSet[MacroDefinition]]

  def add(definition : MacroDefinition) {
    val macroSet = 
      if (macros.contains(definition.name)) 
        macros(definition.name)
      else
        MSet[MacroDefinition]()
    
    macroSet.add(definition)
    macros(definition.name) = macroSet
  }
  
  def get(name : String) : Set[MacroDefinition] = {
    macros.contains(name) match {
      case true => macros(name).toSet
      case false => Set[MacroDefinition]()
    }
  }
}