package uk.co.colinhowe.gleam.compiler.typing

/**
 * Used to check that a class is calling into a generic method correctly by 
 * matching up generic types.


macro x(List<generic T extends Thing> list) with generator(T item) {
  
}
*/
case class GenericType(val typeId : String, val clazz : Class[_ <: Any]) extends Type {
  def canBeAssignedTo(assignee : Type) : Boolean = {
    // Check that the base type is correct
    assignee match {
      case other : GenericType => new SimpleType(clazz).canBeAssignedTo(new SimpleType(other.clazz))
      case _ => throw new UnsupportedOperationException("Not implemented")
    }
  }
}
