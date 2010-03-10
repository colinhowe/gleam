package uk.co.colinhowe.glimpse.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HNodeDefn {
  def apply(
      name : String,
      args : Buffer[PArgDefn] = Buffer(),
      valueType : PType = null,
      restriction : ARestriction = null) = {
    new ANodeDefn(
      new TIdentifier(name), 
      args, 
      valueType,
      restriction
    )
  }
}