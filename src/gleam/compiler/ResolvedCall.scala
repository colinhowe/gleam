package gleam.compiler

object ArgumentSource extends Enumeration {
  type ArgumentSource = Value
  val Call, Cascade, Default = Value
}

import ArgumentSource._

case class ResolvedCall(
  macro : MacroDefinition, 
  argumentSources : Map[String, ArgumentSource]
)