package gleam.compiler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SynchronizedBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.{ Set => MSet}
import scala.actors.Actor
import scala.actors.Actor._
import gleam.compiler.node._

class MacroDefinitionProvider extends Actor {
  val macros = Map.empty[String, MSet[MacroDefinition]]
  val macrosByNode = Map.empty[Node, MacroDefinition]

  start()
  
  private def add(node : Node, definition : MacroDefinition) {
    val macroSet = 
      if (macros.contains(definition.name)) 
        macros(definition.name)
      else
        MSet[MacroDefinition]()
    
    macroSet.add(definition)
    macros(definition.name) = macroSet
    
    macrosByNode(node) = definition
  }
  
  def get(name : String) : Set[MacroDefinition] = {
    macros.contains(name) match {
      case true => macros(name).toSet
      case false => Set[MacroDefinition]()
    }
  }
  
  def getByNode(node : ANodeDefn) : MacroDefinition = macrosByNode(node)
  
  def act() {
    loop {
      react {
        case (node : Node, definition : MacroDefinition) =>
          add(node, definition)
        case Join() =>
          reply(Joined())
          exit
      }
    }
  }
}