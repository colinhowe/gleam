package uk.co.colinhowe.gleam.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HNodeDefn {
  def apply(
      name : String,
      generics : Buffer[PGenericDefn] = Buffer(),
      args : Buffer[PArgDefn] = Buffer(),
      valueType : PType = null,
      restriction : ARestriction = null) = {
    new ANodeDefn(
      new TIdentifier(name), 
      generics,
      args, 
      valueType,
      restriction
    )
  }
}