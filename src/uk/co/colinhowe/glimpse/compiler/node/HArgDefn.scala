package uk.co.colinhowe.glimpse.compiler.node

import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._

object HAArgDefn {
  def apply(
      name : String,
      argType : PType,
      modifiers : Buffer[PModifier] = Buffer[PModifier](),
      default : PExpr = null) : PArgDefn = {
    new AArgDefn(
      modifiers,
      argType,
      new TIdentifier(name),
      default
    )
  }
}