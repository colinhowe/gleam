package gleam

case class IdentifierNotFoundException(identifier : String) extends RuntimeException {
  override def getMessage : String = {
    return "IdentifierNotFoundException[" + identifier + "]"
  }
}