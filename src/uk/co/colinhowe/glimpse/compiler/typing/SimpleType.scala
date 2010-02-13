package uk.co.colinhowe.glimpse.compiler.typing

case class SimpleType(val clazz : Class[_ <: Any]) extends Type {
  def getClazz = clazz

  def canBeAssignedTo(assignee : Type) : Boolean = {
    assignee match {
      case assignee : SimpleType => 
        assignee.clazz.isAssignableFrom(this.clazz)
      case _ => false
    }
  }
}
