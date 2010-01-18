package uk.co.colinhowe.glimpse.compiler.typing

case class SimpleType(val clazz : Class[_ <: Any]) extends Type {
  def getClazz = clazz
}
