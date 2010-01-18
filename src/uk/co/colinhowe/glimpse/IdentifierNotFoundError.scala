package uk.co.colinhowe.glimpse

case class IdentifierNotFoundError(line : Int, identifier : String) extends CompilationError(line)