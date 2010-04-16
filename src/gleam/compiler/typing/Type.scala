package gleam.compiler.typing

abstract class Type {
  def canBeAssignedTo(assignee : Type) : Boolean
}
