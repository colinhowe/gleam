package uk.co.colinhowe.glimpse.compiler.typing

class CompoundType(val clazz : Class[_ <: Any], val innerTypes : List[Type]) extends Type
