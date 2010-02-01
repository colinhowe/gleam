package uk.co.colinhowe.glimpse

case class DynamicMacroMismatchError(line : Int, dynamicMacro : String) extends CompilationError(line)

