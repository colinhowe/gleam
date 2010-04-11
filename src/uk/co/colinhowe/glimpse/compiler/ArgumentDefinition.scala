package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.typing.Type

case class ArgumentDefinition(
    val name : String, 
    val argType : Type, 
    val cascade : Boolean, 
    val hasDefault : Boolean,
    val isRuntimeTyped : Boolean = false)
    