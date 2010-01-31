package uk.co.colinhowe.glimpse

case class DynamicMacroMismatchError(line : Int, macro : String, dynamicMacro : String) extends CompilationError(line)

