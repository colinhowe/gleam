package uk.co.colinhowe.glimpse

case class IdentifierNotFoundException(identifier : String) extends RuntimeException {
  override def getMessage : String = {
    return "IdentifierNotFoundException[" + identifier + "]"
  }
}