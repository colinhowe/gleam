package uk.co.colinhowe.glimpse.compiler.typing

case class CompoundType(val clazz : Class[_ <: Any], val innerTypes : List[Type]) extends Type {
  
  def canBeAssignedTo(assignee : Type) : Boolean = {
    return assignee match {
      case assignee : CompoundType =>
        assignee.clazz.isAssignableFrom(clazz) &&
          innerTypes == assignee.innerTypes
      case _ =>
        false
    }
  }
}
