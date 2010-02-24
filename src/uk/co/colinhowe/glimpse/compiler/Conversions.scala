package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._

trait Conversions {
  implicit def convertAMacroDefnToMacroDefinition(node : AMacroDefn)(implicit typeProvider : TypeProvider, typeNameResolver : TypeNameResolver) = AMacroDefnConversion.convert(node)
}