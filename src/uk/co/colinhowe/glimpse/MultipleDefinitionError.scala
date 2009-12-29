package uk.co.colinhowe.glimpse

case class MultipleDefinitionError(line : Int, macroName : String) extends CompilationError(line)

