package uk.co.colinhowe.glimpse.compiler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SynchronizedBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.{ Set => MSet}
import scala.actors.Actor
import scala.actors.Actor._

class MacroDefinitionProvider extends Actor {
  val macros = Map.empty[String, MSet[MacroDefinition]]

  private def add(definition : MacroDefinition) {
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
  
  def act() {
    loop {
      react {
        case definition : MacroDefinition =>
          add(definition)
        case Join() =>
          reply(Joined())
          exit
      }
    }
  }
}