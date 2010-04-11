package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.node.TIdentifier
import scala.collection.JavaConversions._

object IdentifierConverter {
  def identifierListToString(identifiers : Iterable[TIdentifier]) : String =
    identifierListToString(identifiers, s => s)

  def identifierListToString(identifiers : Iterable[TIdentifier], mapper : String => String) : String = {
    var buffer = new StringBuffer
    for (identifier <- identifiers) {
      buffer.append(mapper(identifier.getText()))
      buffer.append(".")
    }
    return buffer.substring(0, buffer.length - 1)
  }
}