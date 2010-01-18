package uk.co.colinhowe.glimpse

import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SynchronizedBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.JavaConversions._


class MacroDefinitionProvider {
  val macros = Map.empty[String, Set[MacroDefinition]]

  def add(definition : MacroDefinition) {
    val macroSet = 
      if (macros.contains(definition.getName)) 
        macros(definition.getName)
      else
        Set[MacroDefinition]()
    
    macroSet.add(definition)
    macros(definition.getName) = macroSet
  }
  
  def get(name : String) : java.util.Set[MacroDefinition] = {
    macros.contains(name) match {
      case true => macros(name)
      case false => Set[MacroDefinition]()
    }
  }
}