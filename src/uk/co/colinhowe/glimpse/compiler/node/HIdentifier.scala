package uk.co.colinhowe.gleam.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HIdentifier {
  def apply(name : String) = {
    Buffer() ++ name.split("\\.").map(new TIdentifier(_))
  }
}