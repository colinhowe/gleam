package uk.co.colinhowe.gleam

case class IdentifierNotFoundException(identifier : String) extends RuntimeException {
  override def getMessage : String = {
    return "IdentifierNotFoundException[" + identifier + "]"
  }
}