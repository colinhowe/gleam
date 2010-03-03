package uk.co.colinhowe.glimpse.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HMacroDefn {
  def apply(
      name : String,
      modifier : PMacroModifier = null,
      generics : Buffer[PGenericDefn] = Buffer(),
      args : Buffer[PArgDefn] = Buffer(),
      withDefn : PWithDefn = null,
      restriction : ARestriction = null,
      generator : AGenerator = null,
      controller : AController = null) = {
    new AMacroDefn(
      modifier,
      new TIdentifier(name), 
      generics, 
      args, 
      withDefn,
      controller,
      restriction,
      generator
    )
  }
}