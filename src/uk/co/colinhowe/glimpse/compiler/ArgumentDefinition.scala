package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.Type

case class ArgumentDefinition(val name : String, val argType : Type, val cascade : Boolean) {
  
}