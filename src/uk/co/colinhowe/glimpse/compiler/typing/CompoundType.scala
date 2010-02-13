package uk.co.colinhowe.glimpse.compiler.typing

class CompoundType(val clazz : Class[_ <: Any], val innerTypes : java.util.List[Type]) extends Type {
  def canBeAssignedTo(assignee : Type) : Boolean = {
    throw new UnsupportedOperationException("Not implemented")
  }
}
