package uk.co.colinhowe.gleam.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HWithDefn {
  def apply(
      name : String,
      withType : PType,
      args : Buffer[PArgDefn] = Buffer()) : PWithDefn = {
    new AWithDefn(
      new TIdentifier(name),
      withType,
      args
    )
  }
}