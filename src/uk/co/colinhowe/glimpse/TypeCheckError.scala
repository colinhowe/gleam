package uk.co.colinhowe.glimpse

import uk.co.colinhowe.glimpse.compiler.typing.Type

case class TypeCheckError(line : Int, expectedType : Type, actualType : Type) extends CompilationError(line)

