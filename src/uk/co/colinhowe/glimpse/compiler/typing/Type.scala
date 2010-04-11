package uk.co.colinhowe.gleam.compiler.typing

abstract class Type {
  def canBeAssignedTo(assignee : Type) : Boolean
}
