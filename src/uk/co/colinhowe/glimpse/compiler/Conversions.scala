package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.node._

trait Conversions {
  implicit def convertAMacroDefnToMacroDefinition(node : AMacroDefn)(implicit typeProvider : TypeProvider, typeNameResolver : TypeNameResolver) = AMacroDefnConversion.convert(node)
}