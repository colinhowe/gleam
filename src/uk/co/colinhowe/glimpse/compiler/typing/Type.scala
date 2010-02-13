package uk.co.colinhowe.glimpse.compiler.typing

abstract class Type {
  def canBeAssignedTo(assignee : Type) : Boolean
}
