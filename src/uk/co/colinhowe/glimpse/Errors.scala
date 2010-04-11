package uk.co.colinhowe.glimpse

import uk.co.colinhowe.glimpse.compiler.CompilationUnit
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.typing.Type

abstract sealed class CompilationError(val lineNumber : Int)

case class DynamicMacroMismatchError(line : Int, dynamicMacro : String) extends CompilationError(line)

case class MacroNotFoundError(line : Int, name : String, argumentTypes : Map[String, Type], valueType : Type, definitionsFound : Set[MacroDefinition]) extends CompilationError(line)

case class IncompatibleControllerError(line : Int, name : String, controllerFound : Type, controllerNeeded : Type) extends CompilationError(line)

case class MethodNotFoundError(line : Int, identifier : String, arguments : List[Type]) extends CompilationError(line)

case class ParseError(compilationUnit : CompilationUnit, line : Int, column : Int, message : String) extends CompilationError(line)

case class IdentifierNotFoundError(line : Int, identifier : String) extends CompilationError(line)

case class MultipleDefinitionError(line : Int, macroName : String) extends CompilationError(line)

case class TypeCheckError(line : Int, expectedType : Type, actualType : Type) extends CompilationError(line)