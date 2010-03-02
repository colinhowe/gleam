package uk.co.colinhowe.glimpse.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HMacroDefn {
  def apply(
      name : String,
      modifier : PMacroModifier = null,
      generics : Buffer[PGenericDefn] = Buffer(),
      args : Buffer[PArgDefn] = Buffer(),
      withDefn : AWithDefn = null,
      restriction : ARestriction = null,
      generator : AGenerator = null) = {
    new AMacroDefn(
      modifier,
      new TIdentifier(name), 
      generics, 
      args, 
      withDefn,
      null,
      restriction,
      generator
    )
  }
}