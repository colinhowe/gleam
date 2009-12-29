package uk.co.colinhowe.glimpse.compiler.typing

/**
 * Used to check that a class is calling into a generic method correctly by 
 * matching up generic types.


macro x(List<generic T extends Thing> list) with generator(T item) {
  
}
*/
class GenericType(val typeId : String, val clazz : Class[_ <: Any]) extends Type